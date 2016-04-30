
extract_from_report <- function(my_report,name,level=NULL) {
  my_report <- my_report[cumsum(my_report$name==name)==1,,drop=FALSE]

  depth <- my_report[my_report$name==name,"depth"]
  my_report <- my_report[cumsum(my_report$depth <= depth)==1,,drop=FALSE]
  if (!is.null(level))
    my_report <- my_report[my_report$level == level,,drop=FALSE]

  my_report
}


#' Summarize the result of a kraken report
#'
#' @param my_report kraken report
#'
#' @return data.frame with number of raw read, classified at certain levels, etc
#' @export
#'
#' @examples
#'\donotrun{
#'   summarize_kraken_report(krakenres)
#'}
summarize_kraken_report <- function(my_report) {
  my_report <- my_report[!duplicated(my_report$name),]
  row.names(my_report) <- my_report[["name"]]
  unidentified_reads <- my_report["u_unclassified","reads"]
  identified_reads <- my_report["-_root","reads"]
  artificial_reads <- my_report["s_synthetic construct","reads"]
  s_cerevisiae_reads <- my_report["s_Saccharomyces cerevisiae","reads"]
  human_reads <- my_report["s_Homo sapiens","reads"]
  mammal_reads <- my_report["c_Mammalia","reads"]

  #fungal_s_reads <- sum(extract_from_report(my_report,"k_Fungi","S")[,"reads"])
  #eukaryota_s_reads <- sum(extract_from_report(my_report,"d_Eukaryota","S")[,"reads"])
  #eupath_reads <- my_report["d_Eukaryota","reads"] - my_report["d_Eukaryota","reads_stay"] - my_report["-_Opisthokonta","reads"]

  data.frame(
    number_of_raw_reads=unidentified_reads+identified_reads,
    classified_reads=identified_reads,
    mammal_reads=mammal_reads,
    artificial_reads=artificial_reads,
    unclassified_reads=unidentified_reads,
    microbial_reads=identified_reads-mammal_reads-artificial_reads,
    bacterial_reads=my_report["d_Bacteria","reads"],
    viral_reads=my_report["d_Viruses","reads"],
    fungal_reads=my_report["k_Fungi","reads"]
    #s_cerevisia_reads=s_cerevisiae_reads,
    #eupath_reads=eupath_reads,
    #p_apicomplexa_reads=my_report["p_Apicomplexa","reads"],
    #o_kinetoplastida_reads=my_report["o_Kinetoplastida","reads"],
    #Amoebozoa_reads=my_report["-_Amoebozoa","reads"],
    #Heterolobosea_reads=my_report["c_Heterolobosea","reads"],
    #Fornicata_reads=my_report["-_Fornicata","reads"],
    #nonhuman_reads_at_species_level=sum(my_report$reads[my_report$level=="S"])-my_report["s_Homo sapiens","reads"]-artificial_reads
  )
}
