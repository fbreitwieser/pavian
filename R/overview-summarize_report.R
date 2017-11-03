
extract_from_report <- function(my_report,name,taxRank=NULL) {
  my_report <- my_report[cumsum(my_report$name==name)==1,,drop=FALSE]

  depth <- my_report[my_report$name==name,"depth"]
  my_report <- my_report[cumsum(my_report$depth <= depth)==1,,drop=FALSE]
  if (!is.null(taxRank))
    my_report <- my_report[my_report$taxRank == taxRank,,drop=FALSE]

  my_report
}

zero_if_na1 <- function(x) {
  x[is.na(x)] <- 0
  x
}

protist_taxids <- c("-_Diplomonadida"=5738,
                    "-_Amoebozoa"=554915,
                    "-_Alveolata"=33630)



#' Summarize the result of a metagenomics report
#'
#' @param my_report kraken report
#'
#' @return data.frame with number of raw read, classified at certain taxRanks, etc
#' @export
#'
#' @examples
#'\dontrun{
#'   summarize_kraken_report(krakenres)
#'}
summarize_report <- function(my_report) {
  my_report <- my_report[!duplicated(my_report$name),]
  row.names(my_report) <- my_report[["name"]]
  unidentified_reads <- my_report["u_unclassified","cladeReads"]
  identified_reads <- my_report["-_root","cladeReads"]
  artificial_reads <- zero_if_na1(my_report["s_synthetic construct","cladeReads"])
  human_reads <- zero_if_na1(my_report["s_Homo sapiens","cladeReads"])
  chordate_reads <- zero_if_na1(my_report["p_Chordata","cladeReads"])
  root_reads <- zero_if_na1(my_report["-_root","taxonReads"])

  #fungal_s_reads <- sum(extract_from_report(my_report,"k_Fungi","S")[,"cladeReads"])
  #eukaryota_s_reads <- sum(extract_from_report(my_report,"d_Eukaryota","S")[,"cladeReads"])
  #eupath_reads <- my_report["d_Eukaryota","cladeReads"] - my_report["d_Eukaryota","taxonReads"] - my_report["-_Opisthokonta","cladeReads"]

  data.frame(
    number_of_raw_reads=unidentified_reads+identified_reads,
    classified_reads=identified_reads,
    chordate_reads=chordate_reads,
    artificial_reads=artificial_reads,
    unclassified_reads=unidentified_reads,
    microbial_reads=identified_reads-chordate_reads-artificial_reads-root_reads,
    bacterial_reads=zero_if_na1(my_report["d_Bacteria","cladeReads"]) + zero_if_na1(my_report["k_Bacteria","cladeReads"]), ## MetaPhLan reports bacteria as kingdom; Kraken as domain. Sum them
    viral_reads=zero_if_na1(my_report["d_Viruses","cladeReads"]) + zero_if_na1(my_report["k_Viruses","cladeReads"]), ## same as for Bacteria
    fungal_reads=zero_if_na1(my_report["k_Fungi","cladeReads"]),
    protozoan_reads=sum(zero_if_na1(my_report[names(protist_taxids),"cladeReads"]))
    #s_cerevisia_reads=s_cerevisiae_reads,
    #eupath_reads=eupath_reads,
    #p_apicomplexa_reads=my_report["p_Apicomplexa","cladeReads"],
    #o_kinetoplastida_reads=my_report["o_Kinetoplastida","cladeReads"],
    #Amoebozoa_reads=my_report["-_Amoebozoa","cladeReads"],
    #Heterolobosea_reads=my_report["c_Heterolobosea","cladeReads"],
    #Fornicata_reads=my_report["-_Fornicata","cladeReads"],
    #nonhuman_reads_at_species_taxRank=sum(my_report$cladeReads[my_report$taxRank=="S"])-my_report["s_Homo sapiens","cladeReads"]-artificial_reads
  )
}

#' Summarize list of reports
#'
#' @param reports list of reports
#'
#' @return summary of list of reports
#' @export
summarize_reports <- function(reports) {
  do.call(rbind, lapply(reports, pavian::summarize_report))
}

