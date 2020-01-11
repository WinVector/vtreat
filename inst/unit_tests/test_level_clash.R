

test_level_clash <- function() {
  # confirm levels are being encoded correctly (even after name collision)

  d <- data.frame(
    time = c(rep(">= 37 weeks", 3), rep("< 37 weeks", 5)),
    y = c(rep(1,5), rep(0, 3)),
    stringsAsFactors = FALSE
  )
  
  tp <- designTreatmentsN(d, "time", "y", verbose = FALSE)

  d_treated <- prepare(tp, d, check_for_duplicate_frames = FALSE)
  d_treated$time <- d$time
  
  # print(d_treated)
  # unclass(tp$treatments[[1]])
  RUnit::checkTrue("time_lev_x_lt_37_weeks" %in% colnames(d_treated))
  RUnit::checkTrue("time_lev_x_gt_eq_37_weeks" %in% colnames(d_treated))
  RUnit::checkTrue(isTRUE(all.equal(d_treated$time_lev_x_lt_37_weeks,
                                     1 - d_treated$time_lev_x_gt_eq_37_weeks)))
  RUnit::checkTrue(isTRUE(all.equal(d_treated$time_lev_x_lt_37_weeks == 1,
                                    d_treated$time == "< 37 weeks")))
  
  
  # force a collision by using fact . and _ map together
  # (and are problems at the start of a name).
  d <- data.frame(
    time = c(rep(".37 weeks", 3), rep("_37 weeks", 5)),
    y = c(rep(1,5), rep(0, 3)),
    stringsAsFactors = FALSE
  )
  
  tp <- designTreatmentsN(d, "time", "y", verbose = FALSE)
  
  d_treated <- prepare(tp, d, check_for_duplicate_frames = FALSE)
  d_treated$time <- d$time
  
  # print(d_treated)
  # unclass(tp$treatments[[1]])
  RUnit::checkTrue("time_lev_x_37_weeks" %in% colnames(d_treated))
  RUnit::checkTrue("time_lev_x_37_weeks_1" %in% colnames(d_treated))
  RUnit::checkTrue(isTRUE(all.equal(d_treated$time_lev_x_37_weeks,
                                    1 - d_treated$time_lev_x_37_weeks_1)))
  is_diag_2 <- function(t) {
    isTRUE(all.equal(dim(t), c(2, 2))) &&
    ((t[1,1]==0) && (t[2,2]==0)) || ((t[1,2]==0) && (t[2,1]==0))
  }
  RUnit::checkTrue(is_diag_2(table(d_treated$time_lev_x_37_weeks, d$time)))
  RUnit::checkTrue(is_diag_2(table(d_treated$time_lev_x_37_weeks_1, d$time)))
                                    
  invisible(NULL)
}
