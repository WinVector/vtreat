

test_level_clash <- function() {
  # confirm levels are being encoded correctly (even after name collision)

  d <- data.frame(
    time = c(rep(">= 37 weeks", 3), rep("< 37 weeks", 5)),
    y = c(rep(1,5), rep(0, 3)),
    stringsAsFactors = FALSE
  )
  
  tp <- designTreatmentsN(d, "time", "y", verbose = FALSE)

  d_treated <- d %.>% tp
  d_treated$time <- d$time
  
  # print(d_treated)
  # unclass(tp$treatments[[1]])
  RUnit::checkTrue("time_lev_x_lt_37_weeks" %in% colnames(d_treated))
  RUnit::checkTrue("time_lev_x_gt_eq_37_weeks" %in% colnames(d_treated))
  RUnit::checkTrue(isTRUE(all.equal(d_treated$time_lev_x_lt_37_weeks,
                                     1 - d_treated$time_lev_x_gt_eq_37_weeks)))
  RUnit::checkTrue(isTRUE(all.equal(d_treated$time_lev_x_lt_37_weeks == 1,
                                    d_treated$time == "< 37 weeks")))
  
  invisible(NULL)
}
