

#' Patch new columns into a data frame.
#' 
#' Add columns from new_frame to orig_frame, replacing
#' any columns with matching names.
#' 
#' @param orig_frame data.frame
#' @param new_frame data.frame
#' @return merged data.frame
#' 
#' @examples 
#' 
#' patch_in_frame(data.frame(x = 1, y = 2), data.frame(x = 3, z = 4))
#' 
#' @export
#' 
patch_in_frame <- function(orig_frame, new_frame) {
  if(ncol(new_frame)<=0) {
    return(orig_frame)
  }
  trimed_cols <- setdiff(colnames(orig_frame),
                         colnames(new_frame))
  if(length(trimed_cols)<=0) {
    return(new_frame)
  }
  trimmed <- orig_frame[, trimed_cols, drop = FALSE]
  cbind(trimmed, new_frame)
}
