#' @export

## Data processing for Doench 2016 Rule Set 2
## Extracting info from Test Set
## Will output a data frame with all features listed in the study
Doench_2016_processing <- function(seqlist) {
  ## Adds a reference sequence to help with one hot encoding if there is only one sgRNA
  ## Get single nucleotide position features
  ## Splits each sgRNA into individual nucleotides and adds them to a data frame
  for (x in 1:length(seqlist)) {
    split_sgRNA_df <- as.data.frame(stringr::str_split(seqlist, "", simplify = TRUE))
  }
  nuc_list <- c("A", "C", "G", "T")
  for (x in 1:length(nuc_list)){
    ref_sing <- t(as.matrix((rep(nuc_list[x], 30))))
    split_sgRNA_df <- rbind(split_sgRNA_df, ref_sing)
  }
  ## Getting list of names for split_sgRNA_df and applying it
  split_names <- c()
  for (x in 1:30) {
    split_name <- paste("snt_", x, sep = "")
    split_names[length(split_names)+1] <- split_name
  }
  colnames(split_sgRNA_df) <- split_names
  ## vtreat method of one hot encoding
  split_sgRNA_tplan <- vtreat::designTreatmentsZ(split_sgRNA_df, split_names, minFraction= 0, verbose=FALSE)
  ohe_split_sgRNA_df <- vtreat::prepare(split_sgRNA_tplan, split_sgRNA_df)
  ## Getting list of names for one hot encoded columns
  ohe_split_names <- c()
  for (y in 1:30) {
    for (z in 1:4) {
      ohe_split_name <- paste("snt_", y, "_lev_x_", nuc_list[z], sep = "")
      ohe_split_names[length(ohe_split_names)+1] <- ohe_split_name
    }
  }
  ohe_split_names <- as.vector(ohe_split_names)
  ohe_split_sgRNA_df <- ohe_split_sgRNA_df[ohe_split_names]
  ohe_split_sgRNA_df <- ohe_split_sgRNA_df[-c((nrow(ohe_split_sgRNA_df)-3):nrow(ohe_split_sgRNA_df)), ]
  ## End of single nucleotide feature data frame creation

  ## Get dinucleotide position features
  ## Splits each sgRNA into dinucleotides and adds them to a data frame
  di_split_sgRNA_df <- data.frame()
  for (c in 1:length(seqlist)) {
    di_split_sgRNA <- c()
    di_split_sgRNA_list <- c()
    for (x in 1:29){
      di_sgRNA <- substr(seqlist[c], x, 1+x)
      di_split_sgRNA[length(di_split_sgRNA)+1] <- di_sgRNA
    }
    temp_di_split_df <- t(as.data.frame(di_split_sgRNA))
    di_split_sgRNA_df <- rbind(di_split_sgRNA_df, temp_di_split_df)
  }
  colnames(di_split_sgRNA_df) <- c(1:29)
  di_nuc_list <- c("AA", "AC", "AG", "AT", "CA", "CC", "CG", "CT", "GA", "GC", "GG", "GT", "TA", "TC", "TG", "TT")
  for (x in 1:length(di_nuc_list)){
    ref_di <- t(as.data.frame((rep(di_nuc_list[x], 29))))
    colnames(ref_di) <- c(1:29)
    di_split_sgRNA_df <- rbind(di_split_sgRNA_df, ref_di)
  }
  ## Getting list of names for di_split_sgRNA_df and applying it
  di_split_names <- c()
  for (x in 1:29) {
    di_split_name <- paste("dnt_", x, sep = "")
    di_split_names[length(di_split_names)+1] <- di_split_name
  }
  colnames(di_split_sgRNA_df) <- di_split_names
  ## vtreat method of one hot encoding
  di_split_sgRNA_tplan <- vtreat::designTreatmentsZ(di_split_sgRNA_df, di_split_names, minFraction= 0, verbose=FALSE)
  ohe_di_split_sgRNA_df <- vtreat::prepare(di_split_sgRNA_tplan, di_split_sgRNA_df)
  ## Getting list of names for one hot encoded columns
  ohe_di_split_names <- c()
  for (y in 1:29) {
    for (z in 1:16) {
      ohe_di_split_name <- paste("dnt_", y, "_lev_x_", di_nuc_list[z], sep = "")
      ohe_di_split_names[length(ohe_di_split_names)+1] <- ohe_di_split_name
    }
  }
  ohe_di_split_sgRNA_df <- ohe_di_split_sgRNA_df[ohe_di_split_names]
  ohe_di_split_sgRNA_df <- ohe_di_split_sgRNA_df[-c((nrow(ohe_di_split_sgRNA_df)-15):nrow(ohe_di_split_sgRNA_df)), ]
  di_split_sgRNA_df <- di_split_sgRNA_df[-c((nrow(di_split_sgRNA_df)-15):nrow(di_split_sgRNA_df)), ]
  ## End of dinucleotide feature data frame creation

  ## Get position independent single nucleotide counts
  A_count <- c()
  C_count <- c()
  G_count <- c()
  T_count <- c()
  for (x in 1:length(seqlist)) {
    A_count[length(A_count)+1] <- stringr::str_count(seqlist[x], "A")
    C_count[length(C_count)+1] <- stringr::str_count(seqlist[x], "C")
    G_count[length(G_count)+1] <- stringr::str_count(seqlist[x], "G")
    T_count[length(T_count)+1] <- stringr::str_count(seqlist[x], "T")
  }
  single_nuc_count_frame <- data.frame(A_count, C_count, G_count, T_count)
  ## End of position independent single nucleotide features

  ## Get position independent dinucleotide counts
  AA_count <- c()
  AC_count <- c()
  AG_count <- c()
  AT_count <- c()
  CA_count <- c()
  CC_count <- c()
  CG_count <- c()
  CT_count <- c()
  GA_count <- c()
  GC_count <- c()
  GG_count <- c()
  GT_count <- c()
  TA_count <- c()
  TC_count <- c()
  TG_count <- c()
  TT_count <- c()
  for (x in 1:nrow(di_split_sgRNA_df)) {
    AA_count[length(AA_count)+1] <- length(which(di_split_sgRNA_df[x,] == "AA"))
    AC_count[length(AC_count)+1] <- length(which(di_split_sgRNA_df[x,] == "AC"))
    AG_count[length(AG_count)+1] <- length(which(di_split_sgRNA_df[x,] == "AG"))
    AT_count[length(AT_count)+1] <- length(which(di_split_sgRNA_df[x,] == "AT"))
    CA_count[length(CA_count)+1] <- length(which(di_split_sgRNA_df[x,] == "CA"))
    CC_count[length(CC_count)+1] <- length(which(di_split_sgRNA_df[x,] == "CC"))
    CG_count[length(CG_count)+1] <- length(which(di_split_sgRNA_df[x,] == "CG"))
    CT_count[length(CT_count)+1] <- length(which(di_split_sgRNA_df[x,] == "CT"))
    GA_count[length(GA_count)+1] <- length(which(di_split_sgRNA_df[x,] == "GA"))
    GC_count[length(GC_count)+1] <- length(which(di_split_sgRNA_df[x,] == "GC"))
    GG_count[length(GG_count)+1] <- length(which(di_split_sgRNA_df[x,] == "GG"))
    GT_count[length(GT_count)+1] <- length(which(di_split_sgRNA_df[x,] == "GT"))
    TA_count[length(TA_count)+1] <- length(which(di_split_sgRNA_df[x,] == "TA"))
    TC_count[length(TC_count)+1] <- length(which(di_split_sgRNA_df[x,] == "TG"))
    TG_count[length(TG_count)+1] <- length(which(di_split_sgRNA_df[x,] == "TC"))
    TT_count[length(TT_count)+1] <- length(which(di_split_sgRNA_df[x,] == "TT"))
  }
  di_nuc_count_frame <- data.frame(AA_count, AC_count, AG_count, AT_count,
                                   CA_count, CC_count, CG_count, CT_count,
                                   GA_count, GC_count, GG_count, GT_count,
                                   TA_count, TC_count, TG_count, TT_count)

  ## End of position independent dinucleotide features

  ## Get GC content of the 20-mer sgRNA sequence
  ## Three features are extracted, one for the GC content and two more for deviations above and below 10
  GC_content <- c()
  GC_content_grt_10 <- c()
  GC_content_lss_10 <- c()
  for (x in 1:length(seqlist)) {
    sgRNA_20mer <- substr(seqlist[x], 5, 24)
    GC_content[length(GC_content)+1] <- (stringr::str_count(sgRNA_20mer, "G") + stringr::str_count(sgRNA_20mer, "C"))
    if (GC_content[x] > 10) {
      GC_content_grt_10[length(GC_content_grt_10)+1] <- 1
    } else {
      GC_content_grt_10[length(GC_content_grt_10)+1] <- 0
    }
    if (GC_content[x] < 10) {
      GC_content_lss_10[length(GC_content_lss_10)+1] <- 1
    } else {
      GC_content_lss_10[length(GC_content_lss_10)+1] <- 0
    }
  }
  GC_count_frame <- data.frame(GC_content, GC_content_grt_10, GC_content_lss_10)
  ## End of GC count Features

  ## Get features for the two nucleotides surrounding the PAM
  PAM_neighbor <- c()
  for (x in 1:length(seqlist)) {
    PAM_neighbor[length(PAM_neighbor)+1] <- paste(substr(seqlist[x], 25, 25), substr(seqlist[x], 28, 28), sep = "")
  }
  PAM_neighbor <- c(PAM_neighbor, di_nuc_list)
  ## Put list into a data frame
  PAM_neighbor_df <- as.data.frame(PAM_neighbor)
  ## vtreat method of one hot encoding
  PAM_neighbor_tplan <- vtreat::designTreatmentsZ(PAM_neighbor_df, colnames(PAM_neighbor_df), minFraction= 0, verbose=FALSE)
  ohe_PAM_neighbor_df <- vtreat::prepare(PAM_neighbor_tplan, PAM_neighbor_df)
  ## Getting list of names for one hot encoded columns
  ohe_PAM_neighbor_names <- c()
  for (z in 1:16) {
    ohe_PAM_neighbor_name <- paste("PAM_neighbor_lev_x_", di_nuc_list[z], sep = "")
    ohe_PAM_neighbor_names[length(ohe_PAM_neighbor_names)+1] <- ohe_PAM_neighbor_name
  }
  ohe_PAM_neighbor_df <- ohe_PAM_neighbor_df[ohe_PAM_neighbor_names]
  ohe_PAM_neighbor_df <- ohe_PAM_neighbor_df[-c((nrow(ohe_PAM_neighbor_df)-15):nrow(ohe_PAM_neighbor_df)), ]
  ## End of PAM neighbor features

  ## Get Thermodynamic features function
  ## Four Thermodynamic features are extracted, one for the melting temp (Tm) of the 30-mer,
  ## one for the Tm of 5 nt proximal to the PAM, one for the Tm of the 8 nt proximal to the previous 5 nt (away from PAM),
  ## and one for the Tm of the 5 nt proximal to the previous 8 nt (again away from the PAM)
  NN_Thermo <- function (seq) {
    delta_h <- c()
    delta_s <- c()
    ## Assigns starting values for terminal A/T and G/C
    if ((substr(seq, 1, 1) == "G") || (substr(seq, 1, 1) == "C")) {
      delta_h[length(delta_h)+1] <- -0.1
      delta_s[length(delta_s)+1] <- 2.8
    }
    if ((substr(seq, 1, 1) == "A") || (substr(seq, 1, 1) == "T")) {
      delta_h[length(delta_h)+1] <- -2.3
      delta_s[length(delta_s)+1] <- -4.1
    }
    if ((substr(seq, nchar(seq), nchar(seq)) == "G") || (substr(seq, nchar(seq), nchar(seq)) == "C")) {
      delta_h[length(delta_h)+1] <- -0.1
      delta_s[length(delta_s)+1] <- 2.8
    }
    if ((substr(seq, nchar(seq), nchar(seq)) == "A") || (substr(seq, nchar(seq), nchar(seq)) == "T")) {
      delta_h[length(delta_h)+1] <- -2.3
      delta_s[length(delta_s)+1] <- -4.1
    }
    R <- 1.987 # Universal gas constant in Cal/degrees C*Mol
    dnac <- 50 # Dna concentration
    k <- (dnac/4)*1e-9
    di_nuc_list <- c("AA", "AC", "AG", "AT", "CA", "CC", "CG", "CT", "GA", "GC", "GG", "GT", "TA", "TC", "TG", "TT")
    NN_delta_h <- c(6.82, 10.2, 7.6, 9.38, 10.44, 12.22, 10.64, 10.48, 12.44, 14.88, 13.39, 11.4, 7.69, 13.3, 10.5, 6.6)
    NN_delta_s <- c(19, 26.2, 19.2, 26.7, 26.9, 29.7, 26.7, 27.1, 32.5, 36.9, 32.7, 29.5, 20.5, 35.5, 27.8, 18.4)
    Bioseq <- Biostrings::DNAString(seq)
    for (x in 1:length(di_nuc_list)){
      NN_count <- Biostrings::countPattern(di_nuc_list[x], Bioseq)
      if (NN_count != 0) {
        delta_h[length(delta_h)+1] <- NN_count * NN_delta_h[x]
        delta_s[length(delta_s)+1] <- NN_count * NN_delta_s[x]
      }
    }
    # Melting temperature calculations
    ds <- -(sum(delta_s)-0.368*(nchar(seq)-1)*log(50/1e3)) # Assumes a salt concentration of 50
    dh <- -(sum(delta_h))
    tm <- ((1000*(dh))/(ds)+(R*log(k)))-273.15
    tm
  }
  # Breaks the sgRNA sequences into their appropriate regions
  seqlist20_24 <- c()
  seqlist12_19 <- c()
  seqlist7_11 <- c()
  for (x in 1:length(seqlist)) {
    seqlist20_24[length(seqlist20_24)+1] <- substr(seqlist[x], 20, 24)
    seqlist12_19[length(seqlist12_19)+1] <- substr(seqlist[x], 12, 19)
    seqlist7_11[length(seqlist7_11)+1] <- substr(seqlist[x], 7, 11)
  }
  # Uses function to create four lists of the Tm for each region of the thermodynamic features
  Tm <- unlist(lapply(seqlist, NN_Thermo))
  Tm_20_24 <- unlist(lapply(seqlist20_24, NN_Thermo))
  Tm_12_19 <- unlist(lapply(seqlist12_19, NN_Thermo))
  Tm_7_11 <- unlist(lapply(seqlist7_11, NN_Thermo))
  Tm_frame <- data.frame(Tm, Tm_20_24, Tm_12_19, Tm_7_11)

  # Creating a single data frame to store all data
  final_data <- data.frame(ohe_split_sgRNA_df, ohe_di_split_sgRNA_df, single_nuc_count_frame, di_nuc_count_frame, GC_count_frame, ohe_PAM_neighbor_df, Tm_frame)

  # Sets the columns that should be removed (nucleotide features that have to do with the PAM, thus no variation)
  dropcolumns <- c("snt_26_lev_x_A", "snt_26_lev_x_C", "snt_26_lev_x_G", "snt_26_lev_x_T",
                   "snt_27_lev_x_A", "snt_27_lev_x_C", "snt_27_lev_x_G","snt_27_lev_x_T",
                   "dnt_25_lev_x_AA", "dnt_25_lev_x_AC", "dnt_25_lev_x_AT",
                   "dnt_25_lev_x_CA", "dnt_25_lev_x_CC", "dnt_25_lev_x_CT",
                   "dnt_25_lev_x_GA", "dnt_25_lev_x_GC", "dnt_25_lev_x_GT",
                   "dnt_25_lev_x_TA", "dnt_25_lev_x_TC", "dnt_25_lev_x_TT",
                   "dnt_26_lev_x_AA", "dnt_26_lev_x_AC", "dnt_26_lev_x_AG", "dnt_26_lev_x_AT",
                   "dnt_26_lev_x_CA", "dnt_26_lev_x_CC", "dnt_26_lev_x_CG", "dnt_26_lev_x_CT",
                   "dnt_26_lev_x_GA", "dnt_26_lev_x_GC", "dnt_26_lev_x_GG", "dnt_26_lev_x_GT",
                   "dnt_26_lev_x_TA", "dnt_26_lev_x_TC", "dnt_26_lev_x_TG", "dnt_26_lev_x_TT",
                   "dnt_27_lev_x_AA", "dnt_27_lev_x_AC", "dnt_27_lev_x_AG", "dnt_27_lev_x_AT",
                   "dnt_27_lev_x_CA", "dnt_27_lev_x_CC", "dnt_27_lev_x_CG", "dnt_27_lev_x_CT",
                   "dnt_27_lev_x_TA", "dnt_27_lev_x_TC", "dnt_27_lev_x_TG", "dnt_27_lev_x_TT")
  final_data <- final_data[ , !(names(final_data) %in% dropcolumns)]
}
