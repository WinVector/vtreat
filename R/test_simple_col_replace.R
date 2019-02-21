
test_simple_col_replace <- function() {
  d <- wrapr::build_frame(
    "x1"    , "x2"         , "x3", "y" |
      1       , "a"          , 6   , 10  |
      NA_real_, "b"          , 7   , 20  |
      3       , NA_character_, 8   , 30  )
  plan1 <- vtreat::design_missingness_treatment(d)
  r <- vtreat::prepare(plan1, d)
  expect <- c("x1", "x1_isBAD", "x2", "x3", "y")
  RUnit::checkEquals(expect, sort(colnames(r)))
  
  invisible(NULL)
}