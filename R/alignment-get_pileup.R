#' Get the pileup-statistics from a bam file using Rsamtools
#'
#' Returns per-base pileup statistics, or a moving average.
#' TODO: Non-overlapping mean is probably the better idea, here,
#' as only one value per bin is reported
#'
#' @param bam_file character(1) or BamFile; BAM file path.
#' @param top_n If not NULL, return at most the pileups of that many sequences. integer(1).
#' @param min_mapq Minimum mapping quality.
#' @param ... Additional arguments for \code{Rsamtools::pileup}
#'
#' @return \code{data.frame} with sequence names, positions, strand and count information
get_pileup <- function(bam_file, min_mapq = 0, top_n = NULL, ...) {
  req(requireNamespace("Rsamtools"))

  pileup <- Rsamtools::pileup(bam_file,
                              pileupParam=Rsamtools::PileupParam(max_depth=2500, min_base_quality=0, min_mapq=min_mapq,
                                          min_nucleotide_depth=1, min_minor_allele_depth=0,
                                          distinguish_strands=TRUE, distinguish_nucleotides=FALSE,
                                          ignore_query_Ns=TRUE, include_deletions=TRUE, include_insertions=FALSE,
                                          left_bins=NULL, query_bins=NULL, cycle_bins=NULL)
  )

  if (!is.null(top_n)) {
    best_seqs <-
      utils::tail(sort(tapply(
        pileup$count, pileup$seqnames, sum, na.rm = TRUE
      )), top_n)

    pileup <- subset(pileup, seqnames %in% names(best_seqs))
  }
  pileup <- droplevels(pileup)

  ## disregard nucleotide info - maybe change this in future
  pileup <- plyr::ddply(pileup, c("seqnames","strand","pos"), function(x) c(count=sum(x$count)))
  covered_bp <- tapply(pileup$pos,pileup$seqnames,function(x) length(unique(x)))
  sum_count <- tapply(pileup$count,pileup$seqnames,sum)

  seq_lengths <- get_seqlengths(bam_file)

  attr(pileup,"covered_bp") <- covered_bp
  attr(pileup,"sum_count") <- sum_count
  pileup
}

#' Get number of aligned reads from a bam file
#'
#' @param bam_file character(1) or BamFile; BAM file path.
#' @param min_mapq Minimum mapping quality
#'
#' @return Number of reads for each sequence.
get_nreads <- function(bam_file, min_mapq=0) {
  req(requireNamespace("Rsamtools"))
  p2 <- Rsamtools::ScanBamParam(what = c("qname","rname", "strand", "pos", "qwidth", "mapq"))
  bam <- as.data.frame(Rsamtools::scanBam(bam_file, param = p2)[[1]]) %>% dplyr::filter(mapq >= min_mapq)

  res <- tapply(bam$qname, bam$rname, function(x) length(unique(x)))
  res[is.na(res)] <- 0
  res
}

get_bam2 <- function(bam_file) {
  req(requireNamespace("Rsamtools"))
  p2 <- Rsamtools::ScanBamParam(what = c("qname","rname", "strand", "pos", "qwidth", "mapq"))
  res <- as.data.frame(Rsamtools::scanBam(bam_file, param = p2)[[1]])
  res[order(res$rname, res$strand, res$pos), ]
}

#' Get sequence lengths from a bam file
#'
#' @param bam_file character(1) or BamFile; BAM file path.
#'
#' @return named vector with lengths of each sequence.
get_seqlengths <- function(bam_file) {
  req(requireNamespace("Rsamtools"))
  bamHeader <- Rsamtools::scanBamHeader(bam_file)
  bamHeader[[1]][["targets"]]
}
