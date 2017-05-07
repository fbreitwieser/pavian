

#setClassUnion("matrixOrNull", c("matrix", "NULL"))
setClassUnion("matrixOrNull", c("sparseMatrix", "NULL"))
setClassUnion("data.frameOrNull", c("data.frame", "NULL"))


setClass(Class="pavian",
         representation=representation(
           counts_table="matrixOrNull",
           sample_data="data.frameOrNull",
           tax_tree="data.frameOrNull"
         ),
         prototype=prototype(counts_table=NULL, tax_tree=NULL, sample_data=NULL)
)

setMethod("show", "pavian", function(object){
  str(object,max.level = 2)
})

as.pavian <- function(x, ...) UseMethod("as.pavian", x)
as.pavian.phyloseq <- function(x) as(x, "pavian")
as.pavian.MRExperiment <- function(x) as(x, "pavian")

pavian <- function(counts_table, sample_data, tax_tree, summarize_otu_counts = TRUE) {
  tax_tree <- tax_tree(otu_tax_table(GlobalPatterns))
  counts_table <- otu_table(GlobalPatterns)

  if (isTRUE(summarize_otu_counts)) {
    taxid_to_otu <- do.call(rbind, lapply(seq_along(tax_tree$OTUs), function(x) {
      tax <- tax_tree$TaxID[x]
      if (is.null)
      colSums(counts_table[tax_tree$OTUs[[x]],])


      if (!is.null(tax_tree$OTUs[[x]]))
      cbind(TaxID = tax, OTU = )
    }))
    tax_count_table <- tapply(taxid_to_otu[,"OTU"], taxid_to_otu[,"TaxID"], function(x) {

    })

    str(tax_count_table)
    tax_count_table["ABC",] = 0
    aa <- as(tax_count_table, "sparseMatrix")
    colnames(aa)
    aa["472",] <- 0


  }

  new("pavian",
      counts_table=counts_table, sample_data=sample_data, tax_tree=tax_tree)
}

setAs("phyloseq", "pavian",
      function(from) pavian(counts_table(x),sample_data(x),tax_tree(otu_tax_table(x))))

setAs("MRExperiment", "pavian",
      function(from) pavian(counts_table(x),sample_data(x),tax_tree(otu_tax_table(x))))


counts_table <- function(x,...) UseMethod("counts_table", x)
counts_table.phyloseq <- function(x) {
  if( !taxa_are_rows(x) ){ x <- t(x)}
  as(phyloseq::otu_table(x),"matrix") }
counts_table.MRexperiment <- function(x) { metagenomeSeq::MRcounts(x) }

sample_data <- function(x, ...) UseMethod("sample_data", x)
sample_data.phyloseq <- function(x) { as(phyloseq::sample_data(x), "data.frame") }
sample_data.MRexperiment <- function(x) { pData(x) }

## get a OTU tax table. row.names are OTUs
otu_tax_table <- function(x, ...) UseMethod("otu_tax_table", x)
otu_tax_table.phyloseq <- function(x) { as.data.frame(phyloseq::tax_table(x)) }
otu_tax_table.MRexperiment <- function(x) { fData(x) %>% subset(,-OTU) }   ## TODO: upper-case column names, maybe with stringr

tax_tree <- function(x, ...) UseMethod("tax_tree", x)

tax_tree.data.frame <- function(x) {

  # order taxonomy by column 1, ..., n. as.matrix gives character matrix
  ordered_tax <- as.matrix(unique(x[do.call(order, c(as.list(x), na.last = FALSE)),]))

  otu_taxstrings <- apply(x, 1, function(y) {
    ## disregard NAs at the end
    for (no_na_idx in seq(from=length(y), to=1)) {
      if (!is.na(y[no_na_idx]))  break
    }
    paste(y[seq(from=1, to=no_na_idx)], collapse = ";")
  })

  ## generate a vector of all possible taxistrings (also those not directly assigned to an OTU)
  all_taxstrings <- c(unique(ordered_tax[,1]))
  tax_depth <- rep(1, length(all_taxstrings))
  tax_name <- c(unique(ordered_tax[,1]))
  for (i in seq_len(nrow(ordered_tax))) {
    tax_string <- ordered_tax[i,1]

    ## disregard NAs at the end
    for (no_na_idx in seq(from=ncol(ordered_tax), to=1)) {
      if (!is.na(ordered_tax[i,no_na_idx]))  break
    }
    for (j in seq(from=2, to=no_na_idx)) {
      tax_string <- paste0(tax_string,";",ordered_tax[i,j])
      if (!tax_string %in% all_taxstrings) {
        all_taxstrings <- c(all_taxstrings, tax_string)
        tax_depth <- c(tax_depth, j)
        tax_name <- c(tax_name, ordered_tax[i,j])
      }
    }
  }

  name_to_level <- c(species="S", genus="G", family="F", class="C",
                     order="O", phylum="P", kingdom="K", domain="D")
  tax_name_to_level <- function(x) {
    res <- name_to_level[tolower(x)]
    res[is.na(res)] <- "-"
    res
  }

  taxpaths_to_otus <- tapply(names(otu_taxstrings), otu_taxstrings, c)

  stopifnot(all(names(taxpaths_to_otus) %in% all_taxstrings))

  otus_for_paths <- taxpaths_to_otus[all_taxstrings]

  data.frame(TaxID=seq_along(all_taxstrings), Name = tax_name,
             Depth = tax_depth,
             Level = unname(tax_name_to_level(colnames(ordered_tax)[tax_depth])),
             Path = all_taxstrings,
             nOTUs = sapply(otus_for_paths, length),
             OTUs = I(unname(otus_for_paths)),
             stringsAsFactors = FALSE)
}
