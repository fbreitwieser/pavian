

#' Plot a pileup
#'
#' @param pileup The output from \code{\link{get_pileup}}
#' @param nreads Number of matched reads - displayed in legend text.
#' @param seq_lengths Lengths of reference sequences - used for legend text.
#' @param show_loess Display a locally weighthed smoothed line
#' @param show_step If \code{TRUE}, display data in a stairstep plot (instead of a path)
#' @param nwin Number of points to evaluated to evaluate smoother at, when \code{show_loess} is \code{TRUE}
#' @param text_size Legend text size
#'
#' @return ggplot of pileup
#' @export
#' @import ggplot2
plot_pileup <- function(pileup, nreads, seq_lengths, show_loess, show_step = TRUE, nwin = 1000, text_size = 4) {
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


  g <- ggplot(pileup, aes(x = pos, y = count))
  if (show_step)
    g <- g + geom_step(aes(color = strand),alpha=.8)
  else
    g <- g + geom_path(aes(color = strand),alpha=.8)

  if (isTRUE(show_loess))
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
      label = sprintf("%s / %s bp covered\n%s\n%s",
        sprintf("%s", paste0(bp_formatter(covered_bp))),
        sprintf("%s", bp_formatter(genome_size)),
        sprintf("%s reads", n_reads),
        sprintf("avg cov: %sx", signif(avg_coverage, 2))
      )
    ),
    x = Inf, y = Inf, vjust = "inward", hjust = "inward", ## top-right
    size = text_size,
    data = seq_info_df
  )
}
