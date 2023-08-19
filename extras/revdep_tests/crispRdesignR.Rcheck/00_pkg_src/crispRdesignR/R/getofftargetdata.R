#' @export

getofftargetdata <- function(x){
  off_targ_frame <- data.frame(x[16:27])
  colnames(off_targ_frame) <- c("sgRNA sequence", "Chromosome", "Start", "End", "Mismatches", "Direction", "CFD Scores",
                                "Off-target sequence", "Gene ID", "Gene Name", "Sequence Type", "Exon Number")
  off_targ_frame
}
