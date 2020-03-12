

mk_data <- function(..., nrow, n_noise_var = 0, n_signal_var = 0, n_noise_level = 1000) {
  wrapr::stop_if_dot_args(substitute(list(...)), "mk_data")
  # combination of high-complexity useless variables
  # and low-complexity useful variables
  y = rnorm(n = nrow)
  d = data.frame(const_col = rep('a', nrow),
                 stringsAsFactors = FALSE)
  noise_levels = paste0("nl_", seq_len(n_noise_level))
  for(i in seq_len(n_noise_var)) {
    d[paste0("noise_", i)] = sample(
      noise_levels, replace = TRUE, size=nrow
    )
  }
  signal_levels = list("a" = 1, "b" = -1)
  for(i in seq_len(n_signal_var)) {
    v = paste0("signal_", i)
    d[v] = sample(
      names(signal_levels), replace = TRUE, size = nrow
    )
    vn = signal_levels[d[[v]]]
    y = y + as.numeric(vn)
  }
  return(list('d' = d, 'y' = y))
}
