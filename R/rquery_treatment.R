

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
#' @seealso \code{\link{as_rquery}}
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
#' @seealso \code{\link{as_rquery}}
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
                       unique(c(rqplan$treatmentplan$outcomename, 
                                rqplan$treatmentplan$scoreFrame$varName,
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

#' Convert a vtreat tstep into an rquery operation tree.
#' 
#' @param tstep vtreat treatment plan.
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
#'    rqplan <- as_rquery(treatmentsC)
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
#' @export
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

#' @export
as_rquery.treatmentplan <- function(tstep, 
                                    ...,
                                    var_restriction = NULL) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery")
  if(!("treatmentplan" %in% class(tstep))) {
    stop("vtreat::as_rquery.treatmentplan must be of class treatmentplan")
  }
  res <- list(
    exprs = character(0),
    optree_generators = list(),
    tables = list()
  )
  for(ti in tstep$treatments) {
    ri <- as_rquery(ti, var_restriction = var_restriction)
    if(!is.null(ri)) {
      for(fld in c("exprs", "optree_generators", "tables")) {
        res[[fld]] <- c(res[[fld]], ri[[fld]])
      }
    }
  }
  if(length(res$exprs)>0) {
    exprs <- res$exprs # don't get clobbered by res$exprs <- NULL assignment
    f <- function(d) {
      rquery::extend_se(d, exprs)
    }
    res$optree_generators <- c(
      list(f),
      res$optree_generators)
  }
  res$exprs <- NULL
  res$treatmentplan = tstep
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
    stop("vtreat::rquery_code_categorical resname must be a single string")
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
  names(ctab) <- c(colname, resname)
  code_tab <- name_source()
  ctabd <- rquery::table_source(code_tab, c(colname, resname))
  expr <- resname %:=% paste0("ifelse(is.na(", colname, "), ", na_value, 
                              ", ifelse(is.na(", resname, "), ", new_novel_value, ", ", resname, "))")
  f <- function(d) {
    rquery::natural_join(d, ctabd, jointype = "LEFT", by = colname) %.>%
      rquery::extend_se(., expr)
  }
  tables = list(code_tab = ctab)
  names(tables) <- code_tab
  list(
    exprs = list(),
    optree_generators = f, 
    tables = tables)
}



