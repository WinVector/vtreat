test_bind_frames <- function() {
  d <- data.frame(x = 1, y = 's', stringsAsFactors = FALSE)
  frame_list = list(d, d)
  
  # fn way
  r <- vtreat:::.rbindListOfFrames(frame_list)
  expect_true(is.data.frame(r))
  expect <- data.frame(x = c(1, 1), y = c('s', 's'), stringsAsFactors = FALSE)
  expect_true(isTRUE(all.equal(r, expect)))
  expect_true(is.character(r$y))
  expect_true(!is.factor(r$y))
  
  # base R way
  r <- do.call(base::rbind, c(frame_list, list('stringsAsFactors' = FALSE)))
  expect_true(is.data.frame(r))
  expect <- data.frame(x = c(1, 1), y = c('s', 's'), stringsAsFactors = FALSE)
  expect_true(isTRUE(all.equal(r, expect)))
  expect_true(is.character(r$y))
  expect_true(!is.factor(r$y))
  
  # data.table way
  if(requireNamespace("data.table", quietly = TRUE)) {
    r <- as.data.frame(data.table::rbindlist(frame_list),
                       stringsAsFactor=FALSE)
    expect_true(is.data.frame(r))
    expect <- data.frame(x = c(1, 1), y = c('s', 's'), stringsAsFactors = FALSE)
    expect_true(isTRUE(all.equal(r, expect)))
    expect_true(is.character(r$y))
    expect_true(!is.factor(r$y))
  }

  invisible(NULL)
}


test_bind_frames()
