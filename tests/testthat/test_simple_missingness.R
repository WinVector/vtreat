library('vtreat')

context("rquery simple missingness")

test_that("test_simple_missingness.R: Works As Expected", {
  d <- wrapr::build_frame(
    "x1", "x2", "x3" |
    1   , 4   , "A"  |
    NA  , 5   , "B"  |
    3   , 6   , NA   )

  plan <- design_missingness_treatment(d)
  prepare(plan, d)

  prepare(plan, data.frame(x1=NA, x2=NA, x3="E"))
})
