

#' @importFrom wrapr %:=% %.>%
NULL

# don't look unitialized
. <- NULL

#' Flatten a list of functions onto d.
#' 
#' @param d object (usually a data souce)
#' @param fnlist a list of functions
#' @return fnlist[[length(fnlist)]](flatten_fn_list(d, fnlist[[-length(fnlist)]]) (or d if length(fnlist)<1)
#' 
#' @seealso \code{\link{as_rquery_plan}}
#' 
#' @export
#' 
flatten_fn_list <- function(d, fnlist) {
  for(i in seq_len(length(fnlist))) {
    d <- fnlist[[i]](d)
  }
  d
}

#' Materialize a treated data frame remotely.
#' 
#' @param db a db handle.
#' @param rqplan an query plan
#' @param data_source relop, data source (usually a relop_table_source).
#' @param result_table_name character, table name to land result in
#' @param ... force later arguments to bind by name.
#' @param extracols extra columns to copy.
#' @param temporary logical, if TRUE try to make result temporary.
#' @param overwrite logical, if TRUE try to overwrite result.
#' @param print_sql logical, if TRUE print the SQL.
#' @return description of treated table.
#' 
#' @seealso \code{\link{as_rquery_plan}}
#' 
#' @export
#' 
materialize_treated <- function(db, rqplan, data_source, result_table_name,
                                ...,
                                extracols = NULL,
                                temporary = FALSE,
                                overwrite = TRUE,
                                print_sql = FALSE) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::materialize_treated requires the rquery package.")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::materialize_treated")
  for(ni in names(rqplan$tables)) {
    rquery::rq_copy_to(db, ni, rqplan$tables[[ni]], 
                       overwrite = TRUE, temporary = TRUE)
  }
  ops <- flatten_fn_list(data_source, rqplan$optree_generators)
  selcols <- intersect(rquery::column_names(ops), 
                       unique(c(rqplan$outcomename, 
                                rqplan$newvars,
                                extracols)))
  ops <- rquery::select_columns(ops, selcols)
  if(print_sql) {
    cat(rquery::to_sql(ops, db))
  }
  treated <- rquery::materialize(db, ops, 
                                 table_name = result_table_name,
                                 temporary = temporary,
                                 overwrite = overwrite)
  for(ni in names(rqplan$tables)) {
    rquery::rq_remove_table(db, ni)
  }
  treated
}

as_rquery <- function(tstep, 
                      ...,
                      var_restriction = NULL) {
  UseMethod("as_rquery")
}

as_rquery.vtreatment <- function(tstep, 
                                 ...,
                                 var_restriction = NULL) {
  warning(paste("vtreat::as_rquery not yet implemented for ",
                format(tstep),
                ", class",
                paste(class(tstep), collapse = ", ")))
  NULL
}

#' Convert vtreatment plans into a sequence of rquery read operations.
#' 
#' @param treatmentplans vtreat treatment plan or list of vtreat treatment plan sharing same outcome and outcome type.
#' @param ... not used, force any later arguments to bind to names.
#' @param var_restriction character, if not null restrict to producing these variables.
#' @return list(optree_generator (ordered list of functions), temp_tables (named list of tables))
#' 
#' @examples 
#' 
#' if(requireNamespace("rquery", quietly = TRUE)) {
#'    dTrainC <- data.frame(x= c('a', 'a', 'a', 'b' ,NA , 'b'),
#'                          z= c(1, 2, NA, 4, 5, 6),
#'                          y= c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),
#'                          stringsAsFactors = FALSE)
#'    dTrainC$id <- seq_len(nrow(dTrainC))
#'    treatmentsC <- designTreatmentsC(dTrainC, c("x", "z"), 'y', TRUE)
#'    print(prepare(treatmentsC, dTrainC))
#'    rqplan <- as_rquery_plan(list(treatmentsC))
#'    ops <- flatten_fn_list(rquery::local_td(dTrainC), rqplan$optree_generators)
#'    cat(format(ops))
#'    if(requireNamespace("rqdatatable", quietly = TRUE)) {
#'       treated <- rqdatatable::ex_data_table(ops, tables = rqplan$tables)
#'       print(treated[])
#'    }
#'    if(requireNamespace("DBI", quietly = TRUE) &&
#'       requireNamespace("DBI", quietly = TRUE)) {
#'       db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'       source_data <- rquery::rq_copy_to(db, "dTrainC", dTrainC,
#'                                overwrite = TRUE, temporary = TRUE)
#' 
#'       rest <- materialize_treated(db, rqplan, source_data, "dTreatedC", 
#'                                   extracols = "id",
#'                                   print_sql = FALSE)
#'       resd <- DBI::dbReadTable(db, rest$table_name)
#'       print(resd)
#' 
#'       rquery::rq_remove_table(db, source_data$table_name)
#'       rquery::rq_remove_table(db, rest$table_name)
#'       DBI::dbDisconnect(db)
#'    }
#' }
#' 
#' @seealso \code{\link{materialize_treated}}
#' 
#' @export
#'
as_rquery_plan <- function(treatmentplans, 
                           ...,
                           var_restriction = NULL) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery")
  if("treatmentplan" %in% class(treatmentplans)) {
    treatmentplans <- list(treatmentplans)
  }
  if((!is.list(treatmentplans)) || (length(treatmentplans)<1)) {
    stop("vtreat::as_rquery_plan treatmentplans must be a non-empty list of treatmentplans")
  }
  res <- list(
    exprs = character(0),
    optree_generators = list(),
    tables = list()
  )
  outcomename <- character(0)
  newvars <- character(0)
  for(tstep in treatmentplans) {
    if(!is.null(tstep)) {
      if(class(tstep)!='treatmentplan') {
        stop("vtreat::as_rquery_plan treatmentplans must be a non-empty list of treatmentplans")
      }
      newvarsi <- tstep$scoreFrame$varName
      if(!is.null(var_restriction)) {
        newvarsi <- intersect(newvarsi, var_restriction)
      }
      if(length(newvarsi)>0) {
        outcomename <- unique(c(outcomename, tstep$outcomename))
        if(length(outcomename)!=1) {
          stop("vtreat::as_rquery_plan treatmentplans must all share outcomes")
        }
        if(length(intersect(newvarsi, newvars))>0) {
          stop("vtreat::as_rquery_plan treatmentplans must produce disjoint sets of variables")
        }
        newvars <- c(newvars, newvarsi)
        for(ti in tstep$treatments) {
          ri <- as_rquery(ti, var_restriction = var_restriction)
          if(!is.null(ri)) {
            for(fld in c("exprs", "optree_generators", "tables")) {
              res[[fld]] <- c(res[[fld]], ri[[fld]])
            }
          }
        }
      }
    }
  }
  if(length(res$exprs)>0) {
    exprs <- res$exprs # don't get clobbered by res$exprs <- NULL assignment
    f <- function(d) {
      rquery::extend_se(d, exprs)
    }
    res$optree_generators <- c(
      res$optree_generators,      
      list(f))
  }
  res$exprs <- NULL
  res$treatmentplans = treatmentplans
  res$outcomename = outcomename
  res$newvars = newvars
  res
}

#' Build a function that will re-code a categorical value.
#' 
#' @param colname character, name of column to re-code.
#' @param resname character, name of column to produce.
#' @param coding_levels character, levels to not re-map to 'rare'
#' @param effect_values named map to numeric, levels 
#' @param ... not used, force later arguments to be bound by name.
#' @param levRestriction level restriction object.
#' @param default_value numeric, default value used on non-effect_values matches.
#' @param name_source a wrapr::mk_tmp_name_source()
#' @return function generator for rquery pipeline and advisory table.
#' 
#' @noRd
#' 
rquery_code_categorical <- function(colname, resname,
                                    coding_levels,
                                    effect_values,
                                    ...,
                                    levRestriction = NULL,
                                    default_value = 0.0,
                                    name_source = wrapr::mk_tmp_name_source("vtreat_tmp")) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::rquery_code_categorical requires the rquery package")
  }
  effect_values <- unlist(effect_values)
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::rquery_code_categorical")
  if(length(resname)!=1) {
    stop(paste("vtreat::rquery_code_categorical resname must be a single string",
               colname, "->", resname))
  }
  # work out coding table
  coding_levels <- coding_levels[grep("^x ", as.character(coding_levels))]
  coding_levels <- sort(unique(gsub("^x ", "", coding_levels))) # sort kills NA
  tnum <- 1
  while(TRUE) {
    new_novel_level <- paste0("new_novel_level_", tnum)
    if(!(new_novel_level %in% coding_levels)) {
      break
    }
    tnum <- tnum + 1
  }
  new_novel_value <- as.numeric(effect_values[.preProcCat(new_novel_level, levRestriction)])
  if(is.na(new_novel_value)) {
    new_novel_value <- default_value
  }
  na_value <- as.numeric(effect_values[.preProcCat(NA_character_, levRestriction)])
  if(is.na(na_value)) {
    na_value <- default_value
  }
  ctab <- data.frame(levels = coding_levels,
                     stringsAsFactors = FALSE)
  codes <- .preProcCat(ctab$levels, levRestriction)
  ctab$effect <- as.numeric(effect_values[codes])
  ctab$effect[is.na(ctab$effect)] <- default_value
  if(length(ctab$levels)!=length(unique(ctab$levels))) {
    # should not happen, but let's catch it here so later joins are gauranteed to not blow-up
    stop(paste("vtreat:::rquery_code_categorical encoding levels were not unique, var:",
               colname, "->", resname))
  }
  if(nrow(ctab)>0) {
    names(ctab) <- c(colname, resname)
    code_tab <- name_source()
    ctabd <- rquery::table_source(code_tab, c(colname, resname))
    expr <- resname %:=% paste0("ifelse(is.na(", colname, "), ", na_value, 
                                ", ifelse(is.na(", resname, "), ", new_novel_value, ", ", resname, "))")
    f <- function(d) {
      rquery::natural_join(d, ctabd, jointype = "LEFT", by = colname) 
    }
    tables = list(code_tab = ctab)
    names(tables) <- code_tab
  } else {
    tables <- list()
    f <- list()
    expr <- resname %:=% paste0("ifelse(is.na(", colname, "), ", na_value, ", ", new_novel_value, ")")
  }
  list(
    exprs = expr,
    optree_generators = f, 
    tables = tables)
}



