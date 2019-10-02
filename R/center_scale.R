
#' Center and scale a set of variables.
#' 
#' Center and scale a set of variables. Other columns are passed through.
#' 
#' @param d data.frame to work with
#' @param center named vector of variables to center
#' @param scale named vector of variables to scale
#' @return d with centered and scaled columns altered
#' 
#' @examples 
#' 
#' d <- data.frame(x = 1:5, 
#'                 y = c('a', 'a', 'b', 'b', 'b'))
#' vars_to_transform = "x"
#' t <- base::scale(as.matrix(d[, vars_to_transform, drop = FALSE]), 
#'                  center = TRUE, scale = TRUE)
#' t
#' 
#' centering <- attr(t, "scaled:center")
#' scaling <- attr(t, "scaled:scale")
#' center_scale(d, center = centering, scale = scaling)
#' 
#' @export
#' 
center_scale <- function(d, 
                         center,
                         scale) {
  for(ni in names(center)) {
    d[[ni]] <- d[[ni]] - center[[ni]]
  }
    
  for(ni in names(scale)) {
    si <- scale[[ni]]
    if(all(is.finite(si), si!=0.0)) {
      d[[ni]] <- d[[ni]] / scale[[ni]]
    }
  }
  d
}
