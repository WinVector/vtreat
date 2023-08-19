#' @export

## For this script to work properly, this script must be run from Shiny
sgRNA_design_function <- function(userseq, genomename, gtf, designprogress, userPAM, calloffs, annotateoffs){
  requireNamespace("gbm", quietly = TRUE)
  designprogress$inc(1/10, message = "Finding sgRNA")
  ## Detects whether the user input is a .fasta
  if (isTRUE(stringr::str_detect(userseq, ".fasta"))){
    Biostrings_sequence <- rtracklayer::import(userseq)
    sequence <- as.character(Biostrings_sequence)
  } else { ## Imports as a character string
    sequence <- paste(userseq, collapse = "")
    sequence <- stringr::str_replace_all(sequence, stringr::fixed(" "), "")
    Biostrings_sequence <- Biostrings::DNAString(sequence)
  }
  ## Creates a character string that contains the
  ## complementary sequence (Both in the reverse
  ## direction and with substituted nucleotides)
  Biostrings_rev_seq <- Biostrings::reverseComplement(Biostrings_sequence)
  rev_seq <- as.character(Biostrings_rev_seq)
  ## Create an empty list for the forward sgRNA to go
  sgRNA_list_f <- c()
  ## Create an empty list for the reverse sgRNA to go
  sgRNA_list_r <- c()
  ## Create four empty lists for forward and reverse start and end positions
  sgRNA_f_start <- c()
  sgRNA_f_end <- c()
  sgRNA_r_start <- c()
  sgRNA_r_end <- c()
  ## Sets number that helps determine when to stop looking
  ## for possible sgRNA (This prevents it from choosing
  ## an incomplete sgRNA at the very end of the sequence)
  num_char_in_seq <- nchar(sequence) - 29
  ## Sets the PAM sequence and determines what the program
  ## will describe as a possible sgRNA. Even though most sgRNA is
  ## only 20 nucleotides long, nucleotides surrounding the sgRNA
  ## are used for study-based scoring
  setPAM <- userPAM
  revsetPAM <- Biostrings::reverseComplement(Biostrings::DNAString(userPAM))
  lengthPAM <- nchar(setPAM)
  usesetPAM <- stringr::str_replace_all(as.character(setPAM), "N", ".")
  revusesetPAM <- stringr::str_replace_all(as.character(revsetPAM), "N", ".")
  lengthpostPAM <- (6 - lengthPAM)
  PAM <- paste("........................", usesetPAM, paste(rep(".", lengthpostPAM), sep ="", collapse =""), sep="", collapse ="")
  ## Searches all 23 nt streches in the sequence for
  ## possible matches to the PAM, then puts all 30 nt
  ## matches into a list (including the PAM)
  for (x in 1:num_char_in_seq){
    poss_sgRNA <- substr(sequence, x, 29+x)
    if (stringr::str_detect(poss_sgRNA, PAM) == TRUE){
      sgRNA_list_f[length(sgRNA_list_f)+1] <- poss_sgRNA
      sgRNA_f_start[length(sgRNA_f_start)+1] <- x+4
      sgRNA_f_end[length(sgRNA_f_end)+1] <- x+26
    }
  }
  ## Same as above but with the reverse sequence
  for (x in 1:num_char_in_seq){
    poss_sgRNA <- substr(rev_seq, x, 29+x)
    if (stringr::str_detect(poss_sgRNA, PAM) == TRUE){
      sgRNA_list_r[length(sgRNA_list_r)+1] <- poss_sgRNA
      sgRNA_r_start[length(sgRNA_r_start)+1] <- nchar(rev_seq)-x-25
      sgRNA_r_end[length(sgRNA_r_end)+1] <- nchar(rev_seq)-x-3
    }
  }
  ## Removes any sgRNA that contain degenerate bases
  sgRNA_list_f <- sgRNA_list_f[grepl("[UWSMKRYBDHVNZ]", sgRNA_list_f) == FALSE]
  sgRNA_f_start <- sgRNA_f_start[grepl("[UWSMKRYBDHVNZ]", sgRNA_list_f) == FALSE]
  sgRNA_f_end <- sgRNA_f_end[grepl("[UWSMKRYBDHVNZ]", sgRNA_list_f) == FALSE]
  sgRNA_list_r <- sgRNA_list_r[grepl("[UWSMKRYBDHVNZ]", sgRNA_list_r) == FALSE]
  sgRNA_r_start <- sgRNA_r_start[grepl("[UWSMKRYBDHVNZ]", sgRNA_list_r) == FALSE]
  sgRNA_r_end <- sgRNA_r_end[grepl("[UWSMKRYBDHVNZ]", sgRNA_list_r) == FALSE]
  ## Creates list for all sgRNA to go
  sgRNA_list <- c(sgRNA_list_f, sgRNA_list_r)
  if (is.null(sgRNA_list) == FALSE) {
    sgRNA_start <- c(sgRNA_f_start, sgRNA_r_start)
    sgRNA_end <- c(sgRNA_f_end, sgRNA_r_end)
    ## Creates a list with only the sgRNA sequences (no PAMs or
    ## flanking sequence)
    breakseq <- function(seqlist){
      stringr::str_sub(seqlist, 5, 24)
    }
    sgRNA_seq <- sapply(sgRNA_list, breakseq)
    ## Creates a list with only the PAM sequences
    breakPAM <- function(seqlist){
      stringr::str_sub(seqlist, 25, (24+nchar(setPAM)))
    }
    sgRNA_PAM <- sapply(sgRNA_list, breakPAM)
    ## Makes a list of all of the sgRNA sequences with their PAM
    sgRNA_with_PAM <- paste(sgRNA_seq, sgRNA_PAM, sep = "")
    ## Makes a list of whether sgRNA are forward or reverse
    sgRNA_fow <- rep("+", each = length(sgRNA_list_f))
    sgRNA_rev <- rep("-", each = length(sgRNA_list_r))
    sgRNA_fow_or_rev <- c(sgRNA_fow, sgRNA_rev)
    ## Find GC percentage for each sgRNA and puts that data into
    ## a list called "GCinstance"
    FindGC <- function(seqlist){
      ((stringr::str_count(as.character(seqlist), "G") + stringr::str_count(as.character(seqlist), "C")) / 20)
    }
    GCinstance <- sapply(sgRNA_seq, FindGC)
    ## Find homopolmers
    Findhomopolymer <- function(seqlist){
      stringr::str_detect(seqlist, "TTTT|AAAA|GGGG|CCCC")
    }
    Homopolymerdetect <- sapply(sgRNA_seq, Findhomopolymer)
    designprogress$inc(1/10)
    ## Detect Self complementarity
    self_comp_list <- c()
    ## Region of sgRNA backbone that is vulnerable to forming hairpins
    backbone_area <- Biostrings::DNAString("AGGCTAGTCCGT")
    revcomp_backbone <- Biostrings::reverseComplement(backbone_area)
    ## Creates a function to detect the GC content of 4 bp regions of the sgRNA
    SpeFindGC <- function(seqlist){
      ((stringr::str_count(seqlist, "G") + stringr::str_count(seqlist, "C")) / 4)
    }
    ## Process that detects hairpins within the sgRNA or with the backbone
    for (j in 1:length(sgRNA_seq)){
      testDNA <- Biostrings::DNAString(sgRNA_seq[j])
      revcompDNA <- Biostrings::reverseComplement(testDNA)
      individ_comp_list <- c()
      for (r in 1:17) {
        test_region <- substr(testDNA, r, r+3)
        if (SpeFindGC(test_region) >= .5) {
          if (r <= 10) {
            compcount <- Biostrings::countPattern(test_region, Biostrings::reverseComplement(Biostrings::DNAString(substr(testDNA, r+7, length(testDNA)))))
            individ_comp_list[length(individ_comp_list)+1]  <- compcount
          }
          compcount <- Biostrings::countPattern(test_region, revcomp_backbone)
          individ_comp_list[length(individ_comp_list)+1]  <- compcount
        } else {
          individ_comp_list[length(individ_comp_list)+1] <- 0
        }
      }
      self_comp_list[length(self_comp_list)+1] <- sum(as.numeric(individ_comp_list))
    }
    ## Self Complementary check done
    ## Assign a study-based efficiency score based on the Doench 2016 paper and data
    ## Efficiency Score
    processed_efficiency_data <- Doench_2016_processing(sgRNA_list)
    Efficiency_Score <- stats::predict(Rule_Set_2_Model, processed_efficiency_data, n.trees = 500)
    ## Round that efficiency score to three decimal places
    Efficiency_Score <- round(Efficiency_Score, 3)
    ## Study-based efficiency score done
    ## Creates a list that notates any warnings respective to each sgRNA
    Notes <- c()
    for (R in 1:length(sgRNA_seq)) {
      Individ_Notes <- c()
      if (GCinstance[R] >=.8) {
        Individ_Notes[length(Individ_Notes)+1] <- "GC"
      } else if (GCinstance[R] >=.3) {
        Individ_Notes[length(Individ_Notes)+1] <- "GC"
      }
      if (Homopolymerdetect[R] == TRUE) {
        Individ_Notes[length(Individ_Notes)+1] <- "HP"
      }
      if (self_comp_list[R] > 0) {
        Individ_Notes[length(Individ_Notes)+1] <- "SC"
      }
      if (Efficiency_Score[R] < 0.5) {
        Individ_Notes[length(Individ_Notes)+1] <- "LE"
      }
      if (is.null(Individ_Notes)) {
        Individ_Notes[length(Individ_Notes)+1] <- "N/A"
      }
      Notes[length(Notes)+1] <- paste(Individ_Notes, sep = ", ", collapse = ", ")
    }
    ## Ends the function and outputs data if off-target searching is skipped
    if (calloffs == "no_off") {
      designprogress$inc(amount = 7/10)
      mm0_list <- rep("NA", each = length(sgRNA_list))
      mm1_list <- rep("NA", each = length(sgRNA_list))
      mm2_list <- rep("NA", each = length(sgRNA_list))
      mm3_list <- rep("NA", each = length(sgRNA_list))
      mm4_list <- rep("NA", each = length(sgRNA_list))
      ## Creates data table with all available sgRNA data
      sgRNA_data <- data.frame(sgRNA_seq, sgRNA_PAM, sgRNA_fow_or_rev, sgRNA_start, sgRNA_end, GCinstance, Homopolymerdetect, self_comp_list, Efficiency_Score, mm0_list, mm1_list, mm2_list, mm3_list, mm4_list, Notes)
      ## Set the names of each column
      colnames(sgRNA_data) <- c("sgRNA sequence", "PAM sequence", "Direction", "Start", "End", "GC content", "Homopolymer", "Self Complementary", "Efficiency Score", "MM0", "MM1", "MM2", "MM3", "MM4", "Notes")
      sgRNA_data <- sgRNA_data[order(-sgRNA_data$`Efficiency Score`),]
      ## Creates an empty data table for off-target annotation
      all_offtarget_info <- data.frame("NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA")
      colnames(all_offtarget_info) <- c("sgRNA sequence", "Chromosome", "Start", "End", "Mismatches", "Direction", "CFD Score", "Off-target sequence", "Gene ID", "Gene Name", "Sequence Type", "Exon Number")
      data_list <- c("sgRNA_data" = sgRNA_data, "all_offtarget_info" = all_offtarget_info)
      data_list
    } else {
      designprogress$inc(amount = 1/10, message = "Checking for off-targets")
      ## Check for off-targets in the genome
      ## Creates Function that converts all sgRNAs into a format readable
      ## by Biostrings
      multiple_DNAString <- function(seqlist){
        Biostrings::DNAString(seqlist)
      }
      Biostrings_sgRNA <- lapply(sgRNA_with_PAM, multiple_DNAString)
      ## Define genome
      usegenome <- utils::getFromNamespace(genomename, genomename)
      seqnames <- GenomeInfoDb::seqnames(usegenome)
      ## Creates a series of lists to store the incoming mismatch information
      mm0_list <- c()
      mm1_list <- c()
      mm2_list <- c()
      mm3_list <- c()
      mm4_list <- c()
      off_start <- c()
      off_end <- c()
      off_direction <- c()
      off_sgRNAseq <- c()
      off_offseq <- c()
      off_chr <- c()
      off_mismatch <- c()
      nseq <- length(seqnames)
      ## Creates a list of acceptable "NGG" PAMs
      PAM_test_list <- c("GG", "AG", "CG", "GA", "GC", "GT", "TG")
      rev_PAM_test_list <- c("CC", "CT", "CG", "TC", "GC", "AC", "CA")
      for (seqname in seqnames) {
        subject <- usegenome[[seqname]]
        #chrom_as_char <- as.character(subject)
        designprogress$inc(amount = (1/nseq)*.5, message = paste("Checking for Off-Targets in", seqname, sep = " "))
        chrmm0_list <- c()
        chrmm1_list <- c()
        chrmm2_list <- c()
        chrmm3_list <- c()
        chrmm4_list <- c()
        revchrmm0_list <- c()
        revchrmm1_list <- c()
        revchrmm2_list <- c()
        revchrmm3_list <- c()
        revchrmm4_list <- c()
        for (pattern in Biostrings_sgRNA) {
          ### usepattern <- DNAString(paste(substr(as.character(pattern), 1, 20), setPAM, sep ="", collapse =""))
          usepattern <- Biostrings::DNAString(substr(as.character(pattern), 1, 20))
          ## Searches for off-targets in the forward strand
          off_info <- Biostrings::matchPattern(usepattern, subject, max.mismatch = 4, min.mismatch = 0, fixed = TRUE)
          if (length(off_info) > 0) {
            off_info_full <- IRanges::Views(subject, BiocGenerics::start(off_info), BiocGenerics::end(off_info)+lengthPAM)
            if (setPAM == "NGG") {
              off_info_position <- which(substr(as.character(off_info_full), 22, 23) %in% PAM_test_list)
              off_info <- off_info[off_info_position]
              off_info_full <- off_info_full[off_info_position]
            } else {
              off_info_position <- which(stringr::str_detect(substr(as.character(off_info_full), 21, 20+lengthPAM), usesetPAM))
              off_info <- off_info[off_info_position]
              off_info_full <- off_info_full[off_info_position]
            }
          }
          mis_info <- IRanges::elementNROWS(Biostrings::mismatch(usepattern, off_info))
          if (setPAM == "NGG") {
            if (length(off_info) > 0) {
              seqs_w_4mm <- which(mis_info == 4)
              seqs_w_off_PAM <- which(substr(as.character(off_info_full), 22, 23) %in% c("AG", "CG", "GA", "GC", "GT", "TG"))
              discard_offs <- intersect(seqs_w_4mm, seqs_w_off_PAM)
              if (length(discard_offs) != 0) {
                off_info <- off_info[-discard_offs]
                off_info_full <- off_info_full[-discard_offs]
                mis_info <- mis_info[-discard_offs]
              }
            }
          }
          ## Searches for off-targets in the reverse strand
          rev_pattern <- Biostrings::reverseComplement(usepattern)
          rev_off_info <- Biostrings::matchPattern(rev_pattern, subject, max.mismatch = 4, min.mismatch = 0, fixed = TRUE)
          if (length(rev_off_info) > 0) {
            rev_off_info_full <- IRanges::Views(subject, (BiocGenerics::start(rev_off_info)-lengthPAM), BiocGenerics::end(rev_off_info))
            if (setPAM == "NGG") {
              rev_off_info_position <- which(substr(as.character(rev_off_info_full), 1, 2) %in% rev_PAM_test_list)
              rev_off_info <- rev_off_info[rev_off_info_position]
              rev_off_info_full <- rev_off_info_full[rev_off_info_position]
            } else {
              rev_off_info_position <- which(stringr::str_detect(substr(as.character(rev_off_info_full), 1, lengthPAM), revusesetPAM))
              rev_off_info <- rev_off_info[rev_off_info_position]
              rev_off_info_full <- rev_off_info_full[rev_off_info_position]
            }
          }
          rev_mis_info <- IRanges::elementNROWS(Biostrings::mismatch(rev_pattern, rev_off_info))
          if (setPAM == "NGG") {
            if (length(rev_mis_info) > 0) {
              rev_seqs_w_4mm <- which(rev_mis_info == 4)
              rev_seqs_w_off_PAM <- which(substr(as.character(rev_off_info_full), 1, 2) %in% c("CT", "CG", "TC", "GC", "AC", "CA"))
              rev_discard_offs <- intersect(rev_seqs_w_4mm, rev_seqs_w_off_PAM)
              if (length(rev_discard_offs) != 0) {
                rev_off_info <- rev_off_info[-rev_discard_offs]
                rev_off_info_full <- rev_off_info_full[-rev_discard_offs]
                rev_mis_info <- rev_mis_info[-rev_discard_offs]
              }
            }
          }
          if (length(off_info) > 0) {
            for (f in 1:length(off_info)) {
              off_start[length(off_start)+1] <- BiocGenerics::start(off_info)[f]
              off_end[length(off_end)+1] <- BiocGenerics::end(off_info)[f]+lengthPAM
              off_direction[length(off_direction)+1] <- "+"
              off_chr[length(off_chr)+1] <- seqname
              off_mismatch[length(off_mismatch)+1] <- mis_info[f]
              off_sgRNAseq[length(off_sgRNAseq)+1] <- as.character(pattern)
              off_offseq[length(off_offseq)+1] <- as.character(off_info_full[f])
            }
          }
          if (length(rev_off_info) > 0) {
            for (f in 1:length(rev_off_info)) {
              off_start[length(off_start)+1] <- BiocGenerics::start(rev_off_info)[f]-lengthPAM
              off_end[length(off_end)+1] <- BiocGenerics::end(rev_off_info)[f]
              off_direction[length(off_direction)+1] <- "-"
              off_chr[length(off_chr)+1] <- seqname
              off_mismatch[length(off_mismatch)+1] <- rev_mis_info[f]
              off_sgRNAseq[length(off_sgRNAseq)+1] <- as.character(pattern)
              off_offseq[length(off_offseq)+1] <- as.character(rev_off_info_full[f])
            }
          }
          individMM <- c()
          if (length(mis_info) > 0) {
            for (f in 1:length(mis_info)) {
              individMM[length(individMM)+1] <- mis_info[f]
            }
            chrmm0_list[length(chrmm0_list)+1] <- sum(individMM == 0)
            chrmm1_list[length(chrmm1_list)+1] <- sum(individMM == 1)
            chrmm2_list[length(chrmm2_list)+1] <- sum(individMM == 2)
            chrmm3_list[length(chrmm3_list)+1] <- sum(individMM == 3)
            chrmm4_list[length(chrmm4_list)+1] <- sum(individMM == 4)
          } else {
            chrmm0_list[length(chrmm0_list)+1] <- 0
            chrmm1_list[length(chrmm1_list)+1] <- 0
            chrmm2_list[length(chrmm2_list)+1] <- 0
            chrmm3_list[length(chrmm3_list)+1] <- 0
            chrmm4_list[length(chrmm4_list)+1] <- 0
          }
          individMM <- c()
          if (length(rev_mis_info) > 0) {
            for (f in 1:length(rev_mis_info)) {
              individMM[length(individMM)+1] <- rev_mis_info[f]
            }
            revchrmm0_list[length(revchrmm0_list)+1] <- sum(individMM == 0)
            revchrmm1_list[length(revchrmm1_list)+1] <- sum(individMM == 1)
            revchrmm2_list[length(revchrmm2_list)+1] <- sum(individMM == 2)
            revchrmm3_list[length(revchrmm3_list)+1] <- sum(individMM == 3)
            revchrmm4_list[length(revchrmm4_list)+1] <- sum(individMM == 4)
          } else {
            revchrmm0_list[length(revchrmm0_list)+1] <- 0
            revchrmm1_list[length(revchrmm1_list)+1] <- 0
            revchrmm2_list[length(revchrmm2_list)+1] <- 0
            revchrmm3_list[length(revchrmm3_list)+1] <- 0
            revchrmm4_list[length(revchrmm4_list)+1] <- 0
          }
        }
        if (is.null(mm0_list)) {
          mm0_list <- chrmm0_list + revchrmm0_list
          mm1_list <- chrmm1_list + revchrmm1_list
          mm2_list <- chrmm2_list + revchrmm2_list
          mm3_list <- chrmm3_list + revchrmm3_list
          mm4_list <- chrmm4_list + revchrmm4_list
        } else {
          mm0_list <- chrmm0_list + mm0_list + revchrmm0_list
          mm1_list <- chrmm1_list + mm1_list + revchrmm1_list
          mm2_list <- chrmm2_list + mm2_list + revchrmm2_list
          mm3_list <- chrmm3_list + mm3_list + revchrmm3_list
          mm4_list <- chrmm4_list + mm4_list + revchrmm4_list
        }
      } ## Off Target search done
      ## Calculates off-target scores for each off target sequence
      off_model_PAMs <- c("AG", "CG", "GA", "GC", "GT", "TG")
      CFD_PAM_Scores <- data.frame(off_model_PAMs, c(0.259259, 0.107142, 0.069444, 0.022222, 0.016129, 0.038961))
      CFD_Scores <- c()
      for (x in 1:length(off_offseq)) {
        if (off_direction[x] == "-") {
          temporary_off <- Biostrings::DNAString(off_offseq[x])
          temporary_off <- Biostrings::reverseComplement(temporary_off)
          CFDoffsplit <- stringr::str_split(as.character(temporary_off), "", simplify = TRUE)
        } else {
          CFDoffsplit <- stringr::str_split(off_offseq[x], "", simplify = TRUE)
        }
        CFDsgRNAsplit <- stringr::str_split(off_sgRNAseq[x], "", simplify = TRUE)
        individ_scores <- c()
        for (g in 1:20) {
          if (CFDsgRNAsplit[g] != CFDoffsplit[g]) {
            index <- which(CFD_Model_Scores$Position==g & CFD_Model_Scores$sgRNA==CFDsgRNAsplit[g] & CFD_Model_Scores$DNA==CFDoffsplit[g])
            individ_scores[length(individ_scores)+1] <- CFD_Model_Scores[index,4]
          }
        }
        if (setPAM == "NGG") {
          specific_PAM <- (paste(CFDoffsplit[22], CFDoffsplit[23], sep = ""))
          if (isTRUE(specific_PAM != "GG")){
            if (specific_PAM %in% off_model_PAMs) {
              PAM_index <- which(off_model_PAMs==specific_PAM)
              individ_scores[length(individ_scores)+1] <- CFD_PAM_Scores[PAM_index,2]
            } else {
              individ_scores[length(individ_scores)+1] <- 0
            }
          }
        }
        if (length(individ_scores) == 0) {
          CFD_Scores[length(CFD_Scores)+1] <- 1
        } else {
          CFDproduct <- 1
          for (x in 1:length(individ_scores)){
            CFDproduct <- prod(individ_scores[x], CFDproduct)
          }
          CFD_Scores[length(CFD_Scores)+1] <- CFDproduct
        }
      }
      CFD_Scores <- round(CFD_Scores, digits = 3)
      ## Decides whether to annotate off-target sequnces or not based on user set parameters
      if (((sum(mm0_list) + sum(mm1_list) + sum(mm2_list) + sum(mm3_list)) == 0) || (annotateoffs == "no_annotate")) {
        designprogress$inc(amount = 1/10, message = "Compiling Data")
        all_offtarget_info <- data.frame(off_sgRNAseq, off_chr, off_start, off_end, off_mismatch, off_direction, CFD_Scores, off_offseq, "NA", "NA", "NA", "NA")
        colnames(all_offtarget_info) <- c("sgRNA sequence", "Chromosome", "Start", "End", "Mismatches", "Direction", "CFD Score", "Off-target sequence", "Gene ID", "Gene Name", "Sequence Type", "Exon Number")
        ## Put lists in data frame
        sgRNA_data <- data.frame(sgRNA_seq, sgRNA_PAM, sgRNA_fow_or_rev, sgRNA_start, sgRNA_end, GCinstance, Homopolymerdetect, self_comp_list, Efficiency_Score, mm0_list, mm1_list, mm2_list, mm3_list, mm4_list, Notes)
        ## Set the names of each column
        colnames(sgRNA_data) <- c("sgRNA sequence", "PAM sequence", "Direction", "Start", "End", "GC content", "Homopolymer", "Self Complementary", "Efficiency Score", "MM0", "MM1", "MM2", "MM3", "MM4", "Notes")
        sgRNA_data <- sgRNA_data[order(-sgRNA_data$`Efficiency Score`),]
        designprogress$inc(1/10)
        data_list <- c("sgRNA_data" = sgRNA_data, "all_offtarget_info" = all_offtarget_info)
        data_list
      } else {
        designprogress$inc(amount = 0, message = "Annotating Off-Targets")
        ## Creates a function that annotates the off-targets called above
        annotate_genome <- function(ochr, ostart, oend, odir, gtf) {
          GenomeInfoDb::seqlevelsStyle(gtf) <- "UCSC"
          seqer <- unlist(ochr)
          starter <- as.numeric(ostart)
          ender <- as.numeric(unlist(oend))
          strander <- unlist(odir)
          off_ranges <- GenomicRanges::GRanges(seqer, IRanges::IRanges(starter, ender), strander)
          olaps <- IRanges::findOverlaps(off_ranges, gtf)
          geneid <- c()
          geneidlist <- c()
          genename <- c()
          genenamelist <- c()
          sequencetype <- c()
          sequencetypelist <- c()
          exonnumber <- c()
          exonnumberlist <- c()
          S4Vectors::mcols(off_ranges)$gene_id <- c()
          for (p in 1:length(off_ranges)) {
            if (p %in% S4Vectors::queryHits(olaps)) {
              geneid <- S4Vectors::mcols(gtf)$gene_id[S4Vectors::subjectHits(olaps[which(p == S4Vectors::queryHits(olaps))])]
              geneid <- unique(geneid)
              geneidlist[length(geneidlist)+1] <- paste(geneid, collapse = ", ")
              genename <- S4Vectors::mcols(gtf)$gene_name[S4Vectors::subjectHits(olaps[which(p == S4Vectors::queryHits(olaps))])]
              genename <- unique(genename)
              genenamelist[length(genenamelist)+1] <- paste(genename, collapse = ", ")
              sequencetype <- S4Vectors::mcols(gtf)$type[S4Vectors::subjectHits(olaps[which(p == S4Vectors::queryHits(olaps))])]
              sequencetype <- unique(sequencetype)
              sequencetypelist[length(sequencetypelist)+1] <- paste(sequencetype, collapse = ", ")
              exonnumber <- S4Vectors::mcols(gtf)$exon_number[S4Vectors::subjectHits(olaps[which(p == S4Vectors::queryHits(olaps))])]
              exonnumber <- unique(exonnumber)
              exonnumber <- exonnumber[-which(is.na(exonnumber))]
              exonnumberlist[length(exonnumberlist)+1] <- paste(exonnumber, collapse = ", ")
            } else {
              geneidlist[length(geneidlist)+1] <- "NA"
              genenamelist[length(genenamelist)+1] <- "NA"
              sequencetypelist[length(sequencetypelist)+1] <- "NA"
              exonnumberlist[length(exonnumberlist)+1] <- "NA"
            }
          }
          S4Vectors::mcols(off_ranges)$gene_id <- geneidlist
          more_off_info <- data.frame(geneidlist, genenamelist, sequencetypelist, exonnumberlist)
          more_off_info
        }
        ## Ensures that all off-targets provided run in the same direction as the sgRNA sequence
        if (("-" %in% off_direction) == TRUE) {
          revcomp_index <- which(off_direction == "-")
          to_be_revcomped <- c(off_offseq[revcomp_index])
          new_offs <- c()
          x <- 1
          for (x in 1:length(to_be_revcomped)) {
            new_offs[length(new_offs)+1] <- as.character(Biostrings::reverseComplement(Biostrings::DNAString(to_be_revcomped[x])))
          }
          for (x in 1:length(revcomp_index)) {
            off_offseq[revcomp_index[x]] <- new_offs[x]
          }
        }
        ## Compiles data frame of all off-target annotations
        more_off_info <- annotate_genome(off_chr, off_start, off_end, off_direction, gtf)
        designprogress$inc(amount = 1/10, message = "Compiling Data")
        ## Complies all extra sgRNA info into a separate data frame
        all_offtarget_info <- data.frame(off_sgRNAseq, off_chr, off_start, off_end, off_mismatch, off_direction, CFD_Scores, off_offseq, more_off_info$geneidlist, more_off_info$genenamelist, more_off_info$sequencetypelist, more_off_info$exonnumberlist)
        colnames(all_offtarget_info) <- c("sgRNA sequence", "Chromosome", "Start", "End", "Mismatches", "Direction", "CFD Scores", "Off-target sequence", "Gene ID", "Gene Name", "Sequence Type", "Exon Number")
        ## Put lists in data frame
        sgRNA_data <- data.frame(sgRNA_seq, sgRNA_PAM, sgRNA_fow_or_rev, sgRNA_start, sgRNA_end, GCinstance, Homopolymerdetect, self_comp_list, Efficiency_Score, mm0_list, mm1_list, mm2_list, mm3_list, mm4_list, Notes)
        ## Set the names of each column
        colnames(sgRNA_data) <- c("sgRNA sequence", "PAM sequence", "Direction", "Start", "End", "GC content", "Homopolymer", "Self Complementary", "Efficiency Score", "MM0", "MM1", "MM2", "MM3", "MM4", "Notes")
        sgRNA_data <- sgRNA_data[order(-sgRNA_data$`Efficiency Score`),]
        designprogress$inc(1/10)
        data_list <- c("sgRNA_data" = sgRNA_data, "all_offtarget_info" = all_offtarget_info)
        data_list
      }
    }
  } else {
    data_list <- data.frame()
  }
}
