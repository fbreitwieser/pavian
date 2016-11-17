
#' Barplot of metagenomics results at a certain rank
#'
#' @param rank_reads \code{data.frame} with all reads at a certain rank with columns '.id', 'name', value
#' @param value Name of column with quantitative values to display
#' @param ylab Label on y axis
#' @param labels Label formatter. See \code{\link[ggplot2]{scale_y_continuous}}.
#'
#' @return Barplot of rank reads
#' @import ggplot2
#' @export
plot_rank_reads <- function(rank_reads,value="perc",ylab="fraction of reads", labels=percent) {
  ggplot(rank_reads[,c(".id","name",value)]) +
    geom_bar(aes_string(x=".id",y=value,fill="name"),stat="identity") +
    scale_y_continuous(labels=labels) + xlab("") + ylab(ylab) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,colour = "black",size=12),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
    ) + ggthemes::scale_fill_gdocs() + guides(fill=guide_legend(title=NULL))
}
