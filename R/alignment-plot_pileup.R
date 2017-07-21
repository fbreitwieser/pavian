

#' Plot a pileup
#'
#' @param pileup The output from \code{\link{get_pileup}}
#' @param seq_info_df Data.frame with information on sequences
#' @param nwin Number of points to evaluated to evaluate smoother at, when \code{show_loess} is \code{TRUE}
#' @param text_size Legend text size
#'
#' @return ggplot of pileup
#' @export
#' @import ggplot2
plot_pileup <- function(pileup, seq_info_df = NULL,
                        nwin = 500, text_size = 5) {
  if (nrow(pileup) == 0)
    return(NULL)


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

  is_avg <- FALSE

  min_pos <- min(pileup$pos,na.rm=T) - 1
  max_pos <- max(pileup$pos,na.rm=T) + 1


  pileup <- plyr::ddply(pileup, c("seqnames","strand"), function(x) {
    requireNamespace("ggplot2")
    
    if (!is.null(nwin) && nrow(x)/nwin > 1) {
      #message("nwin")
      is_avg <<- TRUE
      x <- x[order(x$pos),]
      pos <- round(seq(from=min_pos,to=max_pos, length.out=nwin-1))
      interval_sizes <- pos[-1] - pos[-length(pos)]
      pos <- pos[-length(pos)]

      intervals <- findInterval(x$pos, pos)
      counts <- rep(0, length(pos))
      counts2 <- tapply(x$count, intervals, sum)
      counts[as.numeric(names(counts2))] <- counts2

      counts <- counts / interval_sizes

      return(cbind(pos   = c(min_pos, pos, max_pos),
                   count = c(ifelse(min_pos %in% x$pos, x$count[1], 0), counts,
                             ifelse(max_pos %in% x$pos, x$count[nrow(x)], 0))))
    } else {
      #message("nowin")
      ## Make sure there is a zero before each value, and a zero afterwards
      #if (!is.null(seq_info_df)) {
      #  max_pos <- seq_info_df[x$seqnames[1],"genome_length"]
      #}

      ## find the positions in between that we want to set to zero
      possible_zero_positions <- sort(unique(c(min_pos,x$pos - 1, x$pos + 1,max_pos)))
      possible_zero_positions <- possible_zero_positions[possible_zero_positions >= min_pos & possible_zero_positions <= max_pos]
      pos_count <- stats::setNames(rep(0, length(possible_zero_positions)),
                            possible_zero_positions)

      pos_count[as.character(x$pos)] <- x$count

      return(cbind(pos=as.numeric(names(pos_count)),
                   count=as.numeric(pos_count)))
    }
  }
  )



  g <- ggplot(pileup, aes(x = pos, y = count))
  #if (!isTRUE(show_loess))
  {
    if (!is_avg)
      g <- g + geom_step(aes(color = strand),alpha=.8)
    else
      g <- g + geom_line(aes(color = strand),alpha=.8)
  }

  g <- g +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(pileup$count) * 1.1)) +
    xlab("Position") + ylab("Coverage")

  if (length(unique(pileup$seqnames)) > 1)
    g <- g + facet_wrap( ~ seqnames, scales = "free")

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

  if (!is.null(seq_info_df) && length(seq_info_df) > 0 && is.data.frame(seq_info_df)) {
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

  g + my_gg_theme(theme.size) + scale_color_manual(values=c("-"="#377eb8","+"="#e41a1c"))
}
