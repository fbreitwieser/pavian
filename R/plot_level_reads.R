
#' Barplot of metagenomics results at a certain taxRank
#'
#' @param taxRank_cladeReads \code{data.frame} with all cladeReads at a certain taxRank with columns '.id', 'name', value
#' @param value Name of column with quantitative values to display
#' @param ylab Label on y axis
#' @param labels Label formatter. See \code{\link[ggplot2]{scale_y_continuous}}.
#'
#' @return Barplot of taxRank "cladeReads"
#' @import ggplot2
#' @export
plot_taxRank_cladeReads <- function(taxRank_cladeReads,value="perc",ylab="fraction of reads", labels=percent) {
  ggplot(taxRank_cladeReads[,c(".id","name",value)]) +
    geom_bar(aes_string(x=".id",y=value,fill="name"),stat="identity") +
    scale_y_continuous(labels=labels) + xlab("") + ylab(ylab) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,colour = "black",size=12),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
    ) + guides(fill=guide_legend(title=NULL))
}
