\name{sgRNA_design}
\alias{sgRNA_design}
\title{sgRNA Target Design}
\description{
sgRNA_design returns information to design sgRNA sequences based on a given target sequence, a genome to annotate off-target information, and a genome annoation file (.gtf), to annotate the off-target findings.
}
\usage{
sgRNA_design(userseq, genomename, gtfname, userPAM, calloffs = TRUE, annotateoffs = TRUE)
}
\arguments{
  \item{userseq}{
    The target sequence to generate sgRNA guides for. Can either be a character sequence containing DNA bases or the name of a fasta file in the working directory.
  }
  \item{genomename}{
    The name of a geneome (from the BSgenome package) to check off-targets for.
  }
  \item{gtfname}{The name of a genome annotation file (.gtf) in the working directory to check off-target sequences against.
  }
  \item{userPAM}{An optional argument used to set a custom PAM for the sgRNA. If not set, the function will default to the "NGG" PAM. Warning: Doench efficieny scores are only accurate for the "NGG" PAM.
  }
  \item{calloffs}{If TRUE, the function will search for off-targets in the genome chosen specified by the genomename argument. If FALSE, off-target calling will be skipped.
  }
  \item{annotateoffs}{If TRUE, the function will provide annotations for the off-targets called using the genome annotation file specified by the gtfname argument. If FALSE, off-target annotation will be skipped.
  }
}
\value{
A list containing all data on the generated sgRNA and all off-target information. List items 1 through 15 include information on each individual sgRNA, including the sgRNA sequence itself, PAM, location, direction relative to the target sequence, GC content, homopolymer presence, presence of self-complementarity, off-target matches, predicted efficiency score, and a notes column that summarizes unfavorable sequence features. List items 16 through 27 include all information on off-target matches, including the original sgRNA sequence, off-target sequence, chromosome, location, direction relative to the target sequence, number of mismatches, gene ID, gene name, type of DNA, and exon number.
}
\details{
Important Note: When designing sgRNA for large genomes (billions of base pairs), use short query DNA sequences (under 500 bp). Depending on your hardware checking for off-targets can be quite computationally intensive and may take several hours if not limited to smaller query sequences.
}
\author{
Dylan Beeber
}
\examples{
## Quick example without off-target searching or annotation
testseq <- "GGCAGAGCTTCGTATGTCGGCGATTCATCTCAAGTAGAAGATCCTGGTGCAGTAGG"
usergenome <- "placeholder"
gtfname <- "placeholder"
alldata <- sgRNA_design(testseq, usergenome, gtfname, calloffs = FALSE)

\donttest{
## Designing guide RNA for a target region as a test string, using
## the Saccharomyces Cerevisiae genome and genome annotation file:
requireNamespace("BSgenome.Scerevisiae.UCSC.sacCer3", quietly = TRUE)
testseq <- "GGCAGAGCTTCGTATGTCGGCGATTCATCTCAAGTAGAAGATCCTGGTGCAGTAGG"
usergenome <- BSgenome.Scerevisiae.UCSC.sacCer3::BSgenome.Scerevisiae.UCSC.sacCer3
gtfname <- "Saccharomyces_cerevisiae.R64-1-1.92.gtf.gz"
annotation_file <- system.file("example_data", gtfname, package = "crispRdesignR")
alldata <- sgRNA_design(testseq, usergenome, annotation_file)

## Designing guide RNA for a target region as a text file, using
## the Saccharomyces Cerevisiae genome and genome annotation file,
## while switching genome annotation off:
testseq <- system.file("example_data", "ExampleDAK1seq.txt", package = "crispRdesignR")
alldata2 <- sgRNA_design(testseq, usergenome, annotation_file, annotateoffs = FALSE)
}
}
