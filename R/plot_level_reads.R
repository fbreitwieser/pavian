
#' Plot report content in form of a barplot
#'
#' @param level_reads
#' @param value
#' @param ylab
#' @param labels
#'
#' @return barplot
#' @export
#'
plot_level_reads <- function(level_reads,value="perc",ylab="fraction of reads", labels=percent) {
  library(ggplot2)
  library(scales)
  require(ggthemes)

  ggplot(level_reads[,c(".id","name",value)]) +
    geom_bar(aes_string(x=".id",y=value,fill="name"),stat="identity") +
    scale_y_continuous(labels=labels) + xlab("") + ylab(ylab) +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5,colour = "black",size=12),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")
    ) + ggthemes::scale_fill_gdocs() + guides(fill=guide_legend(title=NULL))
}
