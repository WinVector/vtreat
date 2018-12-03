

#' Return variable evaluations.
#'
#'
#' @param sf scoreFrame from from vtreat treatments
#' @return per-original varaible evaluations
#'
#'
#' @export
#' 
variable_values <- function(sf) {
  sf$one <- 1
  res <- data.frame(rsq = tapply(sf$rsq, sf$origName, max))
  res <- cbind(res, data.frame(count = tapply(sf$one, sf$origName, sum)))
  res <- cbind(res, data.frame(sig = tapply(sf$sig, sf$origName, min)))
  res$sig <- pmin(1, res$sig*res$count)
  res$count <- NULL
  res
}


