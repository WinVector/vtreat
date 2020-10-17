

test_name_munging <- function() {
  # Issue 23 https://github.com/WinVector/vtreat/issues/23
  d <- wrapr::build_frame(
    "mis be hav (%)", "Sepal.Width", "Petal.Length", "Petal.Width", "Species" |
    5.1             , 3.5          , 1.4           , 0.2          , "setosa"  |
    4.9             , 3            , NA            , 0.2          , "setosa"  |
    4.7             , 3.2          , 1.3           , 0.2          , "setosa"  )
  
  t <- designTreatmentsZ(d, names(d), verbose = FALSE)
  d2 <- prepare(t, d, check_for_duplicate_frames=FALSE)
  expect_true(isTRUE(all.equal(make.names(colnames(d2)), colnames(d2))))
  
  ts <- design_missingness_treatment(d)
  ds <- prepare(ts, d)
  expect_true(isTRUE(all.equal(make.names(colnames(ds)), colnames(ds))))
  
  invisible(NULL)
}

test_name_munging()
