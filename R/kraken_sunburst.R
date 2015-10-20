
#' kraken_sunburst: Create a
#'
#' @param krakenres
#' @param log10
#'
#' @return
#' @export
#'
#' @examples
kraken_sunburst <- function(krakenres,log10=FALSE) {

  ## update end nodes if the data.frame was subset
  end_node <- krakenres[,"depth"] >= c(krakenres[,"depth"][-1],0)
  reads_that_go_down <- krakenres[,"reads"] - krakenres[,"reads_stay"]
  not_all_reads_go_down <- !end_node & (reads_that_go_down < c(krakenres[,"depth"][-1],0))
  krakenres[end_node | not_all_reads_go_down,"reads_stay"] <- krakenres[end_node | not_all_reads_go_down,"reads"]
  krakenres$taxonstring[!end_node] <- paste0(krakenres$taxonstring[!end_node],"|end")
  kk <- krakenres[krakenres$reads_stay>=1,]

  ## update taxonstring to be parseable by sunburst.js
  kk$taxonstring <- gsub("-_root|","",kk$taxonstring, fixed=TRUE)
  kk$taxonstring <- gsub("-","_",kk$taxonstring)
  kk$taxonstring <- gsub("|","-",kk$taxonstring,fixed = TRUE)
  kk <- kk[!is.na(kk$taxonstring),]

  if (isTRUE(log10)) {
    kk$counts <- log10(kk$counts)
  }
  sunburst(data.frame(V1=kk$taxonstring,V2=kk$reads_stay),count=TRUE)
}
