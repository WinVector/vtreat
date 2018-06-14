
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

#' Convert a vtreat tstep into an rquery operation tree.
#' 
#' @param tstep vtreat treatment plan
#' @param ... not used, force any later arguments to bind to names.
#' @return list(optree_generator (ordered list of functions), temp_tables (named list of tables))
#' 
#' @examples 
#' 
#' if(requireNamespace("rquery", quietly = TRUE)) {
#'    dTrainC <- data.frame(x= c('a','a','a','b',NA,'b'),
#'                          z= c(1,2,NA,4,5,6),
#'                          y= c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#'    treatmentsC <- designTreatmentsC(dTrainC, colnames(dTrainC),'y',TRUE)
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
#'       rquery::rq_copy_to(db, "dTrainC", dTrainC, overwrite = TRUE, temporary = TRUE)
#'       
#'       for(ni in names(rqplan$tables)) {
#'          rquery::rq_copy_to(db, ni, rqplan$tables[[ni]], overwrite = TRUE, temporary = TRUE)
#'       }
#'       cat(format(rquery::to_sql(ops, db)))
#'       treated <- rquery::execute(db, ops, allow_executor = FALSE)
#'       print(treated)
#'       for(ni in names(rqplan$tables)) {
#'          rquery::rq_remove_table(db, ni)
#'       }
#'       rquery::rq_remove_table(db, "dTrainC")
#'       DBI::dbDisconnect(db)
#'    }
#' }
#' 
#' @export
as_rquery <- function(tstep, 
                      ...) {
  UseMethod("as_rquery")
}

as_rquery.vtreatment <- function(tstep, 
                                 ...) {
  warning(paste("vtreat::as_rquery not yet implemented for ",
                format(tstep),
                ", class",
                paste(class(tstep), collapse = ", ")))
  NULL
}

#' @export
as_rquery.treatmentplan <- function(tstep, 
                                    ...) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery")
  if(!("treatmentplan" %in% class(tstep))) {
    stop("vtreat::as_rquery.treatmentplan must be of class treatmentplan")
  }
  res <- list(
    optree_generators = list(),
    tables = list()
  )
  for(ti in tstep$treatments) {
    ri <- as_rquery(ti)
    if(!is.null(ri)) {
      for(fld in c("optree_generators", "tables")) {
        res[[fld]] <- c(res[[fld]], ri[[fld]])
      }
    }
  }
  res
}



