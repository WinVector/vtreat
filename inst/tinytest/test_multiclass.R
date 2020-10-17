
test_multiclass <- function() {
  # create example data
  set.seed(326346)
  sym_bonuses <- rnorm(3)
  names(sym_bonuses) <- c("a", "b", "c")
  sym_bonuses3 <- rnorm(3)
  names(sym_bonuses3) <- as.character(seq_len(length(sym_bonuses3)))
  n_row <- 1000
  d <- data.frame(x1 = rnorm(n_row),
                  x2 = sample(names(sym_bonuses), n_row, replace = TRUE),
                  x3 = sample(names(sym_bonuses3), n_row, replace = TRUE),
                  y = "NoInfo",
                  stringsAsFactors = FALSE)
  d$y[sym_bonuses[d$x2] > pmax(d$x1, sym_bonuses3[d$x3], runif(n_row))] <- "Large1"
  d$y[sym_bonuses3[d$x3] > pmax(sym_bonuses[d$x2], d$x1, runif(n_row))] <- "Large2"
  
  # define problem
  vars <- c("x1", "x2", "x3")
  y_name <- "y"
  y_levels <- sort(unique(d[[y_name]]))
  
  # build the multi-class cross frame and treatments
  cfe_m <- mkCrossFrameMExperiment(d, vars, y_name)
  sf <- cfe_m$score_frame
  cf <- cfe_m$cross_frame
  prepped <- prepare(cfe_m$treat_m, d, check_for_duplicate_frames=FALSE)
  expect_equal(sort(colnames(prepped)), 
                         sort(colnames(cf)))
  expect_equal(character(0), 
                         setdiff(sf$varName, colnames(cf)))
  expect_equal("y", 
                         setdiff(colnames(cf), sf$varName))
  
  invisible(NULL)
}

test_multiclass()

