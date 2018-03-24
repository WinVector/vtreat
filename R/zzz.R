
vtreat_default_options <- list(
  vtreat.use_data.table_binding = FALSE,
  vtreat.use_dplyr_binding = TRUE
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- setdiff(names(vtreat_default_options), names(op))
  if(length(toset)>0) {
    options(vtreat_default_options[toset])
  }
  invisible()
}
