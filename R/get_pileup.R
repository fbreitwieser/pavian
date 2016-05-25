#' Title
#'
#' @param bam_file
#' @param align_moving_avg
#' @param nwin
#'
#' @return
#' @export
#'
#' @examples
get_pileup <- function(bam_file, align_moving_avg, nwin = 1000) {
  require(Rsamtools)
  require(plyr)

  pileup <- Rsamtools::pileup(bam_file)

  best_seqs <-
    tail(sort(tapply(
      pileup$count, pileup$seqnames, sum, na.rm = TRUE
    )), 20)
  pileup <- subset(pileup, seqnames %in% names(best_seqs))
  pileup <- droplevels(pileup)
  covered_bp <- tapply(pileup$pos,pileup$seqnames,function(x) length(unique(x)))
  sum_count <- tapply(pileup$count,pileup$seqnames,sum)

  seq_lengths <- get_seqlengths(bam_file)
  if (isTRUE(align_moving_avg)) {
    pileup <- ddply(pileup, c("seqnames", "strand"), function(x) {
      genome_length <- seq_lengths[x$seqnames[1]]
      win_size <- genome_length / nwin
      while (win_size < 1) {
        nwin <- nwin / 10
        win_size <- genome_length / nwin
      }

      data.frame(
        seqnames = x$seqnames[1],
        pos = c(1, (win_size) * ((1:nwin) - 1) + win_size / 2, genome_length),
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
    pileup <- ddply(pileup, c("seqnames", "strand"), function(x) {
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


#' Title
#'
#' @param bam_file
#'
#' @return
#' @export
#'
#' @examples
get_nreads <- function(bam_file) {
  require(Rsamtools)
  p2 <- Rsamtools::ScanBamParam(what = c("qname","rname", "strand", "pos", "qwidth"))
  bam <- Rsamtools::scanBam(bam_file, param = p2)
  res <- tapply(bam[[1]]$qname, bam[[1]]$rname, function(x) length(unique(x)))
  res[is.na(res)] <- 0
  res
}

#' Title
#'
#' @param bam_file
#'
#' @return
#' @export
#'
#' @examples
get_seqlengths <- function(bam_file) {
  bamHeader <- Rsamtools::scanBamHeader(bam_file)
  bamHeader[[1]][["targets"]]
}
