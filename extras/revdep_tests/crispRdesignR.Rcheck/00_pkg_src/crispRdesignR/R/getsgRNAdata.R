#' @export

getsgRNAdata <- function(x){
  sgRNA_frame <- data.frame(x[1:15])
  colnames(sgRNA_frame) <- c("sgRNA sequence", "PAM sequence", "Direction", "Start", "End", "GC content",
                                      "Homopolymer", "Self Complementary", "Doench Score", "MM0", "MM1",
                                      "MM2", "MM3", "MM4", "Notes")
  sgRNA_frame
}
