

#' Get all reads from a certain level
#'
#' @param reports List fo kraken reports.
#' @param ... Arguments to \code{subset} reports.
#' @param min_perc Minimum percentage to be included.
#' @param select_names Select by taxons by name instead of \code{min_perc}.
#'
#' @return all reads from a certain level
#' @export
#'
#' @examples
#' # plot_level_reads(get_level_reads(reports,level=="P",min_perc=.05)) + ggtitle("Phylum level")
#' # plot_level_reads(get_level_reads(reports,level=="C",min_perc=.05)) + ggtitle("Clade level")
#' # plot_level_reads(get_level_reads(reports,level=="O",min_perc=.05)) + ggtitle("Order level")
#' # plot_level_reads(get_level_reads(reports,level=="F" & name != "f_Hominidae",min_perc=.05)) + ggtitle("Family level (w/o human)")
get_level_reads <- function(reports,...,min_perc = .1,select_names=NULL) {
  ssubset <- deparse(substitute(...))
  level_reads <- plyr::ldply(reports, function(r) {
    if (ssubset != "NULL")
      r <- subset(r, eval(parse(text = ssubset)))

    r$perc = r$reads / sum(r$reads)
    r[,c("name","reads","perc")]
  })

  max_perc <- tapply(level_reads$perc,level_reads$name,max)
  if (is.null(select_names))
    sel_names <- names(max_perc)[max_perc > min_perc]
  else
    sel_names <- names(max_perc)[names(max_perc) %in% select_names]

  level_reads <- level_reads[order(level_reads$.id,level_reads$name,decreasing=TRUE),]
  level_reads <-
    plyr::ddply(level_reads,".id",function(x) {
      xx <- x[x$name %in% sel_names,]
      if (sum(!x$name %in% sel_names) > 0) {
        xx <- rbind(xx,
                    data.frame(.id=x[1,".id"],name="others",
                               reads=sum(x[!x$name %in% sel_names,"reads"]),
                               perc=sum(x[!x$name %in% sel_names,"perc"]),
                               stringsAsFactors=FALSE))
      }
      xx
    })

  level_reads$name <- sub("^[a-z]_","",level_reads$name)
  level_reads$.id <- factor(level_reads$.id,levels=names(reports))
  level_reads
}
