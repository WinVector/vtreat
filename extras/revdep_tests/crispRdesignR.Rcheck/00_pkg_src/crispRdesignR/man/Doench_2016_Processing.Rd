\name{Doench_2016_processing}
\alias{Doench_2016_processing}
\title{Donech 2016 Processsing}
\description{
Warning: This function is not designed to be directly called by the user. This function is used internally in sgRNA_design() and sgRNA_design_function().

Internal function that encodes all sgRNA sequence information into a data frame. This data frame is then used in conjunction with the Rule_Set_2_Model to predict effciency scores for the generated sgRNA.
}
\usage{
Doench_2016_processing(seqlist)
}
\arguments{
  \item{seqlist}{
    A list of 30-mer sgRNA (as a character string) with the sgRNA sequence spanning from positions 5 to 24.
  }
}
\value{
A data frame containing processed data on the presence of relevant sequence features to the Rule_Set_2_Model for effciency scoring. Includes information on single nucleotide positions, dinucleotide positions, single nucleotide count, dinucleotide count, GC count, PAM neighboring nucleotides, and melting temperatures. Single nulceotide positions, dinucleotide positions, and PAM neighboring nucleotides are all one-hot encoded.
}
\author{
Dylan Beeber
}
