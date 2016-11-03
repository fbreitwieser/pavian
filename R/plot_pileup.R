

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
plot_pileup <- function(pileup, nreads=NULL, seq_lengths=NULL, show_loess=FALSE,
                        show_step = TRUE, nwin = 500, text_size = 4) {
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

  round2 = function(x, n=0) {
    ## R rounds to the nearest even digit
    # from http://stackoverflow.com/questions/12688717/round-up-from-5-in-r
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    z*posneg
  }


  if (!is.null(nwin)) {
    pileup <- plyr::ddply(pileup, c("seqnames","strand"), function(x) {
      step_size <- round(nrow(x)/nwin)
      if (step_size > 1) {
        count2 <- rep(0, nwin)
        for (i in 1:nwin) {
          count2[i] <- sum(x$count[x$pos >= i*step_size & x$pos < (i+1)*step_size])/step_size
        }
        #pos_fact <- round2(x$pos / fact)*fact
        #res <- tapply(x$count, pos_fact, sum)
        #return(cbind(pos=as.numeric(names(res)), count = as.numeric(res)))
        return(cbind(pos=(seq(from=0, to=nwin-1)*step_size + 1), count = count2))
      } else {
        return(cbind(pos=x$pos, count = x$count))
      }

    })

  }

  g <- ggplot(pileup, aes(x = pos, y = count))
  #if (!isTRUE(show_loess))
    {
  if (show_step)
    g <- g + geom_step(aes(color = strand),alpha=.8)
  else
    g <- g + geom_line(aes(color = strand),alpha=.8)
}
  #g <- g + geom_path(aes(color = strand),alpha=.8)

#    g <- g + geom_smooth(aes(color = strand),
                    #n = ceiling(seq_lengths[1]/1000),
#                    span = span,
#                    n = nwin,
#                    se = FALSE,
#                    method = "loess")

  g <- g +
    #scale_y_continuous(expand = c(0, 0)) +
    xlab("Position") + ylab("Coverage") +
    facet_wrap( ~ seqnames, scales = "free")

  bp_formatter <- function(x) {
    if (length(x) == 0 || is.null(x))
      return(x)
    x[!is.na(x) & x < 0] <- 0
    x[!is.na(x)] <- humanreadable(x[!is.na(x)])
    x
  }

  g <- g + scale_x_continuous(labels=bp_formatter, expand = c(0,0))

  geom.text.size = text_size
  theme.size = (14/5) * geom.text.size

  if (length(seq_info_df) > 0 && is.data.frame(seq_info_df)) {
  g <- g + geom_text(
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

  g + theme(strip.background = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour='#D0D0D0',size=.2),
            axis.text = element_text(size = theme.size, colour="black"))
}
