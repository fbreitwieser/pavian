#' Get the pileup-statistics from a bam file using Rsamtools
#'
#' Returns per-base pileup statistics, or a moving average.
#' TODO: Non-overlapping mean is probably the better idea, here,
#' as only one value per bin is reported
#'
#' @param bam_file character(1) or BamFile; BAM file path.
#' @param summarize boolean(1). Return average of \code{nwin} observations.
#' @param nwin integer(1) of number of observations to average.
#' @param top_n If not NULL, return at most the pileups of that many sequences. integer(1).
#' @param ... Additional arguments for \code{Rsamtools::pileup}
#'
#' @return \code{data.frame} with sequence names, positions, strand and count information
#' @export
#'
#' @examples
#' bam_file <- system.file("shinyapp","example-data","CP4-JC_polyomavirus.bam", package = "pavian")
#' head(get_pileup(bam_file))
#' head(get_pileup(bam_file, summarize = TRUE))
get_pileup <- function(bam_file, summarize = FALSE, nwin = 1000, top_n = NULL, ...) {
  pileup <- Rsamtools::pileup(bam_file)

  if (!is.null(top_n)) {
    best_seqs <-
      utils::tail(sort(tapply(
        pileup$count, pileup$seqnames, sum, na.rm = TRUE
      )), top_n)

    pileup <- subset(pileup, seqnames %in% names(best_seqs))
  }
  pileup <- droplevels(pileup)
  covered_bp <- tapply(pileup$pos,pileup$seqnames,function(x) length(unique(x)))
  sum_count <- tapply(pileup$count,pileup$seqnames,sum)

  seq_lengths <- get_seqlengths(bam_file)
  if (isTRUE(summarize)) {
    pileup <- plyr::ddply(pileup, c("seqnames", "strand"), function(x) {
      genome_length <- seq_lengths[x$seqnames[1]]
      win_size <- genome_length / nwin
      while (win_size < 1) {
        nwin <- nwin / 10
        win_size <- genome_length / nwin
      }

      data.frame(
        seqnames = x$seqnames[1],
        pos = c(1, (win_size) * ((1:nwin) - 1) + win_size / 2, genome_length),  ## make positions from 1:genome_length
        strand = x$strand[1],
        nucleotide = NA,
        count = c(
          ifelse(1 %in% x$pos, x$count[x$pos == 1], 0),
          sapply(1:nwin,
                 function(i)
                   sum(x$count[x$pos > (i - 1) * win_size &
                                 x$pos < i * win_size]) / win_size),
          ifelse(genome_length %in% x$pos, x$count[x$pos ==
                                                     genome_length], 0)
        )
      )
    })
  } else {
    pileup <- plyr::ddply(pileup, c("seqnames", "strand"), function(x) {
      genome_length <- seq_lengths[x$seqnames[1]]
      poss <- c(x$pos + 1,x$pos - 1)
      pos_to_set_zero <-
        poss[!poss %in% x$pos & poss < genome_length & poss > 0]

      aa <- rbind(
        x,
        data.frame(
          seqnames = x$seqnames[1],
          pos = pos_to_set_zero,
          strand = x$strand[1],
          nucleotide = NA,
          count = 0
        )
      )
      aa[order(aa$pos),]
    })

  }
  attr(pileup,"covered_bp") <- covered_bp
  attr(pileup,"sum_count") <- sum_count
  pileup
}


#' Get number of aligned reads from a bam file
#'
#' @param bam_file character(1) or BamFile; BAM file path.
#'
#' @return Number of reads for each sequence.
#' @export
#'
#' @examples
#' bam_file <- system.file("shinyapp","example-data","CP4-JC_polyomavirus.bam", package = "pavian")
#' get_nreads(bam_file)
get_nreads <- function(bam_file) {
  p2 <- Rsamtools::ScanBamParam(what = c("qname","rname", "strand", "pos", "qwidth"))
  bam <- Rsamtools::scanBam(bam_file, param = p2)
  res <- tapply(bam[[1]]$qname, bam[[1]]$rname, function(x) length(unique(x)))
  res[is.na(res)] <- 0
  res
}

#' Get sequence lengths from a bam file
#'
#' @param bam_file character(1) or BamFile; BAM file path.
#'
#' @return named vector with lengths of each sequence.
#' @export
#'
#' @examples
#' bam_file <- system.file("shinyapp","example-data","CP4-JC_polyomavirus.bam", package = "pavian")
#' get_seqlengths(bam_file)
get_seqlengths <- function(bam_file) {
  bamHeader <- Rsamtools::scanBamHeader(bam_file)
  bamHeader[[1]][["targets"]]
}
