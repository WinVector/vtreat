

.onAttach <- function(libname, pkgname) {
  vtreat_default_options <- list(
    vtreat.use_data.table_binding = TRUE,
    vtreat.use_clean_suffix = FALSE
  )
  op <- options()
  toset <- setdiff(names(vtreat_default_options), names(op))
  if(length(toset)>0) {
    options(vtreat_default_options[toset])
  }
  invisible()
}
