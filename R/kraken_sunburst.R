
#' kraken_sunburst: Create a
#'
#' @param krakenres kraken report, read with read.krakenres
#'
#' @return sunburst widget
#' @export
#'
kraken_sunburst <- function(krakenres) {
  if (nrow(krakenres) == 0)
    return()

  ## update end nodes if the data.frame was subset.
  ##  end_nodes are those where the following taxonstring does not contain the current
  end_node <- !as.logical(mapply(grepl,
                                 krakenres$taxonstring,
                                 c(krakenres$taxonstring[-1],""),fixed=TRUE))
  sel_reads_stay <- end_node & krakenres[,"reads_stay"] < krakenres[,"reads"]
  krakenres[sel_reads_stay,"reads_stay"] <- krakenres[sel_reads_stay,"reads"]

  ## TODO: consider also updating intermediate nodes, when
  ##  filtering was applied and reads and reads_stay are not actual anymore
  ##  maybe: adding 'other_taxa' entries
  #reads_that_go_down <- krakenres[,"reads"] - krakenres[,"reads_stay"]
  #not_all_reads_go_down <- !end_node & (reads_that_go_down < c(krakenres[,"depth"][-1],0))
  #krakenres[not_all_reads_go_down,"reads_stay"] <- krakenres[sel_reads_stay,"reads"]

  krakenres$taxonstring[!end_node] <- paste0(krakenres$taxonstring[!end_node],"|end")
  kk <- krakenres[krakenres$reads_stay>=1,]

  ## update taxonstring to be parseable by sunburst.js
  kk$taxonstring <- gsub("-_root|","",kk$taxonstring, fixed=TRUE)
  kk$taxonstring <- gsub("-","_",kk$taxonstring)
  kk$taxonstring <- gsub("|","-",kk$taxonstring,fixed = TRUE)
  kk <- kk[!is.na(kk$taxonstring),]

  sunburstR::sunburst(data.frame(V1=kk$taxonstring,V2=kk$reads_stay),count=TRUE)
}
