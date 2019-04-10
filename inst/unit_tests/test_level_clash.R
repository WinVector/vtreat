

test_level_clash <- function() {
  # confirm levels are being encoded correctly (even after name collision)

  d <- data.frame(
    time = c(rep(">= 37 weeks", 3), rep("< 37 weeks", 5)),
    y = c(rep(1,5), rep(0, 3)),
    stringsAsFactors = FALSE
  )
  
  tp <- designTreatmentsN(d, "time", "y")

  d_treated <- d %.>% tp
  d_treated$time <- d$time
  
  # print(d_treated)
  # unclass(tp$treatments[[1]])
  RUnit::checkTrue(isTRUE(all.equal(d_treated$time_lev_x_37_weeks,
                                     1 - d_treated$time_lev_x_37_weeks_1)))
  tbl <- table(d_treated$time_lev_x_37_weeks, d_treated$time)
  RUnit::checkEquals(c(2, 2), dim(tbl))
  diag <- ((tbl[1,1]==0) && (tbl[2,2]==0)) ||  ((tbl[1,2]==0) && (tbl[2,1]==0))
  RUnit::checkTrue(diag)
  
  invisible(NULL)
}
