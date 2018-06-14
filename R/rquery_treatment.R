
#' Convert a vtreat tstep into an rquery operation tree.
#' 
#' @param tstep vtreat treatment plan
#' @param d relop data source (usually of class relop_table_source)
#' @param ... not used, force any later arguments to bind to names.
#' @return rquery optree
#' 
#' @examples 
#' 
#' if(requireNamespace("rquery", quietly = TRUE)) {
#'    dTrainC <- data.frame(x= c('a','a','a','b',NA,'b'),
#'                          z= c(1,2,NA,4,5,6),
#'                          y= c(FALSE,FALSE,TRUE,FALSE,TRUE,TRUE))
#'    treatmentsC <- designTreatmentsC(dTrainC, colnames(dTrainC),'y',TRUE)
#'    ops <- as_rquery(treatmentsC, rquery::local_td(dTrainC))
#'    cat(format(ops))
#'    if(requireNamespace("rqdatatable", quietly = TRUE)) {
#'       treated <- rqdatatable::ex_data_table(ops)
#'       print(treated[])
#'    }
#'    if(requireNamespace("DBI", quietly = TRUE) &&
#'       requireNamespace("DBI", quietly = TRUE)) {
#'       db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'       old_o <- options(list("rquery.rquery_db_executor" = list(db = db)))
#'       cat(format(rquery::to_sql(ops, db)))
#'       treated <- rquery::execute(dTrainC, ops, allow_executor = FALSE)
#'       print(treated)
#'       options(old_o)
#'       DBI::dbDisconnect(db)
#'    }
#' }
#' 
#' @export
as_rquery <- function(tstep, 
                      d,
                      ...) {
  UseMethod("as_rquery")
}

as_rquery.vtreatment <- function(tstep, 
                                 d,
                                 ...) {
  warning(paste("vtreat::as_rquery not yet implemented for ",
                format(tstep),
                ", class",
                paste(class(tstep), collapse = ", ")))
  NULL
}

#' @export
as_rquery.treatmentplan <- function(tstep, 
                                    d,
                                    ...) {
  if(!requireNamespace("rquery", quietly = TRUE)) {
    stop("vtreat::as_rquery.treatmentplan requires the rquery package")
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "vtreat::as_rquery")
  if(!("treatmentplan" %in% class(tstep))) {
    stop("vtreat::as_rquery.treatmentplan must be of class treatmentplan")
  }
  res <- d
  for(ti in tstep$treatments) {
    rs <- as_rquery(ti, res)
    if(!is.null(rs)) {
      res <- rs
    }
  }
  res
}



