
#' Barplot of metagenomics results at a certain level
#'
#' @param level_reads \code{data.frame} with all reads at a certain level with columns '.id', 'name', value
#' @param value Name of column with quantitative values to display
#' @param ylab Label on y axis
#' @param labels Label formatter. See \code{\link[ggplot2]{scale_y_continuous}}.
#'
#' @return Barplot of level reads
#' @import ggplot2
#' @export
plot_level_reads <- function(level_reads,value="perc",ylab="fraction of reads", labels=percent) {
  ggplot(level_reads[,c(".id","name",value)]) +
    geom_bar(aes_string(x=".id",y=value,fill="name"),stat="identity") +
    scale_y_continuous(labels=labels) + xlab("") + ylab(ylab) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,colour = "black",size=12),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
    ) + ggthemes::scale_fill_gdocs() + guides(fill=guide_legend(title=NULL))
}
