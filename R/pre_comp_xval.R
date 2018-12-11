


#' Pre-computed cross-plan (so same split happens each time).
#' 
#' 
#' @param nRows number of rows to split (integer >1).
#' @param nSplits number of groups to split into (ignored).
#' @param splitplan split plan to actually use
#' @return splitplan
#' 
#' @examples
#' 
#' p1 <- oneWayHoldout(3,NULL,NULL,NULL)
#' p2 <- pre_comp_xval(3, 3, p1)
#' p2(3, 3)
#' 
#' @export
pre_comp_xval <- function(nRows, nSplits, splitplan) {
  force(nRows)
  force(nSplits)
  force(splitplan)
  eRows <- nRows
  eSplits <- nSplits
  attr(splitplan, 'splitmethod') <- paste(attr(splitplan, 'splitmethod'),
                                          "( pre-computed", eRows, eSplits, ")")
  f <- function(nRows, nSplits, dframe, y) {
    if(nRows!=eRows) {
      stop("row count mismatch")
    }
    if(nSplits!=eSplits) {
      stop("split count mismatch")
    }
    return(splitplan)
  }
  f
}
