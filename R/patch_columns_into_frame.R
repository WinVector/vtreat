
#' Patch columns into data.frame.
#' 
#' Add columns from new_frame into old_frame, replacing any
#' columns with matching names in orig_frame with values from
#' new_frame.
#' 
#' @param orig_frame data.frame to patch into.
#' @param new_frame data.frame to take replacement columns from.
#' @return patched data.frame
#' 
#' @examples 
#' 
#' orig_frame <- data.frame(x = 1, y = 2)
#' new_frame <- data.frame(y = 3, z = 4)
#' patch_columns_into_frame(orig_frame, new_frame)
#' 
#' @export
#' 
patch_columns_into_frame <- function(orig_frame, new_frame) {
  if(ncol(new_frame)<=0) {
    return(orig_frame)
  }
  trimed_cols <- setdiff(colnames(orig_frame),
                         colnames(new_frame))
  if(length(trimed_cols)<=0) {
    return(new_frame)
  }
  orig_frame <- orig_frame[, trimed_cols, drop = FALSE]
  cbind(orig_frame, new_frame)
}
