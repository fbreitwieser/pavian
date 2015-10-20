
extract.from.report <- function(my.report,name,level=NULL) {
  my.report <- my.report[cumsum(my.report$name==name)==1,,drop=FALSE]

  depth <- my.report[my.report$name==name,"depth"]
  my.report <- my.report[cumsum(my.report$depth <= depth)==1,,drop=FALSE]
  if (!is.null(level))
    my.report <- my.report[my.report$level == level,,drop=FALSE]

  my.report
}


#' Summarize the result of a kraken report
#'
#' @param my.report kraken report
#'
#' @return data.frame with number of raw read, classified at certain levels, etc
#' @export
#'
#' @examples
#' \donotrun{
#'   summarize.kraken.report(krakenres)
#' }
summarize.kraken.report <- function(my.report) {
  my.report <- my.report[!duplicated(my.report$name),]
  row.names(my.report) <- my.report[["name"]]
  unidentified.reads <- my.report["u_unclassified","reads"]
  identified.reads <- my.report["-_root","reads"]
  artificial.reads <- my.report["s_synthetic construct","reads"]
  s.cerevisiae.reads <- my.report["s_Saccharomyces cerevisiae","reads"]
  human.reads <- my.report["s_Homo sapiens","reads"]

  #fungal.s.reads <- sum(extract.from.report(my.report,"k_Fungi","S")[,"reads"])
  #eukaryota.s.reads <- sum(extract.from.report(my.report,"d_Eukaryota","S")[,"reads"])
  eupath.reads <- my.report["d_Eukaryota","reads"] - my.report["d_Eukaryota","reads_stay"] - my.report["-_Opisthokonta","reads"]

  data.frame(
    number.of.raw.reads=unidentified.reads+identified.reads,
    classified.reads=identified.reads,
    unclassified.reads=unidentified.reads,
    human.reads=human.reads,
    artificial.reads=artificial.reads,
    bacterial.reads=my.report["d_Bacteria","reads"],
    viral.reads=my.report["d_Viruses","reads"],
    fungal.reads=my.report["k_Fungi","reads"],
    s.cerevisia.reads=s.cerevisiae.reads,
    eupath.reads=eupath.reads,
    #p_apicomplexa.reads=my.report["p_Apicomplexa","reads"],
    #o_kinetoplastida.reads=my.report["o_Kinetoplastida","reads"],
    #Amoebozoa.reads=my.report["-_Amoebozoa","reads"],
    #Heterolobosea.reads=my.report["c_Heterolobosea","reads"],
    #Fornicata.reads=my.report["-_Fornicata","reads"],
    nonhuman.reads.at.species.level=sum(my.report$reads[my.report$level=="S"])-my.report["s_Homo sapiens","reads"]-artificial.reads
  )
}
