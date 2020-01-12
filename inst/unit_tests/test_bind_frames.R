test_bind_frames <- function() {
  d <- data.frame(x = 1, y = 's', stringsAsFactors = FALSE)
  frame_list = list(d, d)
  
  # fn way
  r <- vtreat:::.rbindListOfFrames(frame_list)
  RUnit::checkTrue(is.data.frame(r))
  expect <- data.frame(x = c(1, 1), y = c('s', 's'), stringsAsFactors = FALSE)
  RUnit::checkTrue(isTRUE(all.equal(r, expect)))
  RUnit::checkTrue(is.character(r$y))
  RUnit::checkTrue(!is.factor(r$y))
  
  # base R way
  r <- do.call(base::rbind, c(frame_list, list('stringsAsFactors' = FALSE)))
  RUnit::checkTrue(is.data.frame(r))
  expect <- data.frame(x = c(1, 1), y = c('s', 's'), stringsAsFactors = FALSE)
  RUnit::checkTrue(isTRUE(all.equal(r, expect)))
  RUnit::checkTrue(is.character(r$y))
  RUnit::checkTrue(!is.factor(r$y))
  
  # data.table way
  if(requireNamespace("data.table", quietly = TRUE)) {
    r <- as.data.frame(data.table::rbindlist(frame_list),
                       stringsAsFactor=FALSE)
    RUnit::checkTrue(is.data.frame(r))
    expect <- data.frame(x = c(1, 1), y = c('s', 's'), stringsAsFactors = FALSE)
    RUnit::checkTrue(isTRUE(all.equal(r, expect)))
    RUnit::checkTrue(is.character(r$y))
    RUnit::checkTrue(!is.factor(r$y))
  }

  invisible(NULL)
}