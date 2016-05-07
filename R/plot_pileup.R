

#' Title
#'
#' @param pileup
#' @param nreads
#' @param align_loess
#' @param nwin
#'
#' @return
#' @export
#'
#' @examples
plot_pileup <- function(bam_file, moving_avg, align_loess, nwin = 1000, text_size = 4) {
  require(Rsamtools)
  require(plyr)
  require(ggplot2)

  pileup <- get_pileup(bam_file, moving_avg)
  nreads <- get_nreads(bam_file)
  seq_lengths <- get_seqlengths(bam_file)

  if (nrow(pileup) == 0)
    return(NULL)

  covered_bp <- attr(pileup,"covered_bp")
  covered_bp[setdiff(names(seq_lengths),names(covered_bp))] <- 0
  sum_count <- attr(pileup,"sum_count")
  sum_count[setdiff(names(seq_lengths),names(sum_count))] <- 0

  seq_info_df <- do.call(rbind,lapply(names(seq_lengths), function(name) {
    data.frame(seqnames=name,
               genome_size=seq_lengths[name],
               avg_coverage=sum_count[name]/seq_lengths[name],
               covered_bp=covered_bp[name],
               n_reads=nreads[name]
               )
  }))

  seq_info_df$perc_covered = seq_info_df$covered_bp / seq_info_df$genome_size


  g <- ggplot(pileup, aes(x = pos, y = count)) +
    geom_step(aes(color = strand),alpha=.8)

  if (isTRUE(align_loess))
    g <-
    g + geom_smooth(aes(color = strand),
                    n = nwin,
                    span = .1,
                    method = "loess")

  g <- g +
    #scale_x_continuous(expand = c(0, 0)) +
    #scale_y_continuous(expand = c(0, 0)) +
    xlab("Position") + ylab("Coverage") +
    facet_wrap( ~ seqnames, scales = "free")

  bp_formatter <- function(x) {
    if (length(x) == 0 || is.null(x))
      return(x)
    x[!is.na(x)] <- humanreadable(x[!is.na(x)])
    x
  }

  g <- g + scale_x_continuous(labels=bp_formatter)


  g + geom_text(
    aes(
      label = sprintf(
        "genome: %sp\ncovered: %sp\navg cov: %.f%%\n# of reads: %s",
        paste0(bp_formatter(genome_size),"b"),
        paste0(bp_formatter(covered_bp),"b"),
        signif(100 * avg_coverage, 2),
        n_reads
      )
    ),
    x = Inf, y = Inf, vjust = "inward", hjust = "inward", ## top-right
    size = text_size,
    data = seq_info_df
  )
}
