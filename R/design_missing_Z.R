

.xform_cat <- function(col, arg) {
  known_values <- arg$known_values
  invalid_mark <- arg$invalid_mark
  col <- as.character(col)
  bads <- is.na(col) | (!(col %in% known_values))
  col[bads] <- invalid_mark
  col
}

.xform_num <- function(col, arg) {
  replacement <- arg$replacement
  col <- as.numeric(col)
  bads <- is.na(col) | is.nan(col) | is.infinite(col)
  col[bads] <- replacement
  col
}

.ind_na <- function(col, arg) {
  col <- as.numeric(col)
  bads <- is.na(col) | is.nan(col) | is.infinite(col)
  v <- rep(0, length(col))
  v[bads] <- 1
  v
}

.xform_zap <- function(col, arg) {
 NULL
}


#' Design a simple treatment plan to indicate missingingness and perform simple imputation.
#' 
#' 
#' @param dframe data.frame to drive design.
#' @param ... not used, forces later arguments to bind by name.
#' @param varlist character, names of columns to process.
#' @param invalid_mark character, name to use for NA levels and novel levels.
#' @param drop_constant_columns logical, if TRUE drop columns that do not vary from the treatment plan.
#' @return simple treatment plan.
#' 
#' @examples 
#' 
#' d <- wrapr::build_frame(
#'   "x1", "x2", "x3" |
#'   1   , 4   , "A"  |
#'   NA  , 5   , "B"  |
#'   3   , 6   , NA   )
#' 
#' plan <- design_missingness_treatment(d)
#' prepare(plan, d)
#' 
#' prepare(plan, data.frame(x1=NA, x2=NA, x3="E"))
#' 
#' @seealso \code{\link{prepare.simple_plan}}
#' 
#' @export
#' 
design_missingness_treatment <- function(dframe, 
                                         ..., 
                                         varlist = colnames(dframe),
                                         invalid_mark = "_invalid_",
                                         drop_constant_columns = FALSE) {
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::design_missingness_treatment")
  force(invalid_mark)
  ops <- list()
  for(ci in varlist) {
    vi <- dframe[[ci]]
    if(is.null(vi)) {
      stop(paste("vtreat::design_missingness_treatment: column", ci, "not found"))
    }
    if(drop_constant_columns) {
      if(!.has.range(vi)) {
         next
      }
    }
    if(is.logical(vi) || (is.numeric(vi) && (!is.factor(vi)))) {
      vi <- as.numeric(vi)
      mean_v <- 0.0
      bads <- is.na(vi) | is.nan(vi) | is.infinite(vi)
      if(any(!bads)) {
        mean_v <- mean(vi[!bads])
      }
      ops <- c(ops, 
               list(
                 list(
                   col = ci,
                   nm = vtreat_make_names(ci), 
                   f = .xform_num, 
                   code = "numeric",
                   args = list(replacement = mean_v))))
      if(any(bads)) {
        ops <- c(ops,
                 list(
                   list(
                     col = ci,
                     nm = vtreat_make_names(paste0(ci, "_isBAD")), 
                     f = .ind_na, 
                     code = "is_bad",
                     args = list())))
      }
    } else {
      vi <- as.character(vi)
      ops <- c(ops, 
               list(
                 list(
                   col = ci,
                   nm = vtreat_make_names(ci), 
                   f = .xform_cat, 
                   code = "cat",
                   args = list(known_values = sort(unique(vi)),
                               invalid_mark = invalid_mark))))
      
    }
  }
  class(ops) <- "simple_plan"
  ops
}

#' Prepare a simple treatment.
#' 
#' @param treatmentplan A simple treatment plan.
#' @param dframe data.frame to be treated.
#' @param ... not used, present for S3 signature consistency.
#' 
#' @examples 
#' 
#' d <- wrapr::build_frame(
#'   "x1", "x2", "x3" |
#'   1   , 4   , "A"  |
#'   NA  , 5   , "B"  |
#'   3   , 6   , NA   )
#' 
#' plan <- design_missingness_treatment(d)
#' prepare(plan, d)
#' 
#' prepare(plan, data.frame(x1=NA, x2=NA, x3="E"))
#' 
#' @seealso \code{\link{design_missingness_treatment}}, \code{\link{prepare}}
#' 
#' @export
#' 
prepare.simple_plan <- function(treatmentplan, dframe,
                                ...) {
  wrapr::stop_if_dot_args(substitute(list(...)), 
                          "vtreat:::prepare.simple_plan")
  res <- dframe
  for(pi in treatmentplan) {
    ci <- pi$col
    res[[ci]] <- NULL
  }
  for(pi in treatmentplan) {
    ci <- pi$col
    vi <- dframe[[ci]]
    if(is.null(vi)) {
      stop(paste("vtreat::prepare.simple_plan: column", ci, " must be in data.frame"))
    }
    vi <- pi$f(vi, pi$args)
    res[[pi$nm]] <- vi
  }
  res
}

#' @export
format.simple_plan <- function(x, ...) { 
  steps <- lapply(
    x, 
    function(xi) {
      data.frame(origName = xi$col,
                 varName = xi$nm,
                 code = xi$code,
                 stringsAsFactors = FALSE)
    })
  steps <- .rbindListOfFrames(steps)
  format(steps)
}

#' @export
as.character.simple_plan <- function (x, ...) {
  format(x, ...)
}

#'
#' Print treatmentplan.
#' @param x treatmentplan
#' @param ... additional args (to match general signature).
#' @export
print.simple_plan <- function(x, ...) { 
  print(format(x), ...) 
}



