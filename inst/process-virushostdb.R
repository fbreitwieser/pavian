
library(dplyr)
library(magrittr)

virushostdb <- read.delim(system.file("virushostdb.tsv.gz",package="pavian"))
id.col <- c("virus.tax.id")
summarize.col <- c("host.name", "host.lineage", "DISEASE")
virushostdb1 <- unique(virushostdb[,c(id.col, summarize.col)])
virushostdb2 <- virushostdb1 %>% group_by_(id.col) %>% summarise(host=paste(host.lineage,host.name, sep="; ", collapse="<br/>\n"), DISEASE=paste(unique(DISEASE), collapse="<br/>\n"))
virushostdb2 <- as.data.frame(virushostdb2)
colnames(virushostdb2)[2] <- "Host"
colnames(virushostdb2)[3] <- "Disease"
rownames(virushostdb2) <- virushostdb2[[1]]
virushostdb2[,1] <- NULL

gz1 <- gzfile("~/projects/pavian/inst/virushostdb1.tsv.gz", "w")
write.table(virushostdb2, file=gz1, sep="\t")
close(gz1)

virushostdb2["10632",]
virushostdb1["10632",]

library(KEGGREST)
listDatabases()
all_genomes <- KEGGREST::keggList("genome")
ss <- strsplit(all_genomes, ";")
my_genomes <- ss[sapply(ss, length)==2]  ## strip those without name and taxid
abr_and_taxid <- t(sapply(strsplit(sapply(my_genomes, "[[", 1), ","), function(x) {
  x[c(1,length(x))]
}))
all_genomes_df <- data.frame(kegg_id=names(my_genomes),
                             name=sapply(my_genomes, "[[", 2),
                             abr=abr_and_taxid[,1],
                             taxID=as.numeric(abr_and_taxid[,2]),
                             row.names="abr")
KEGGREST::listDatabases()
org <- KEGGREST::keggList("organism")
head(org)
org[org[,"organism"]=="spy",]
#ftp://ftp.genome.jp/pub/kegg/medicus/disease/disease
txt <- readLines("~/projects/pavian/inst/kegg-medicus-disease")
res <- KEGGREST:::.flatFileParser(paste(aa,collapse="\n"))
sel_id <- sapply(res,function(x) x$CATEGORY=="Infectious disease")
summary(sel_id)
ids <- res[sel_id]
all_colnames <- unique(unlist(sapply(ids, names)))
bb <- unlist(sapply(ids, function(x) sapply(x,class)))
unique(data.frame(bb,names(bb)))
## reference is always list, otherwise it's characters
cc <- lapply(ids, function(x) {
  x <- x[all_colnames]
  names(x) <- all_colnames
  x[sapply(x, is.null)] <- NA
  pathogens <- x$PATHOGEN[grep("GN:",x$PATHOGEN)]
  pathogens <- strsplit(paste(gsub(".*.GN:(.*).","\\1", pathogens), collapse=" "), " ")[[1]]
  x$PATHOGEN <- all_genomes_df[pathogens,]
  x
})
ids[269]
lapply(cc,"[[","PATHOGEN")
lapply(ids,"[[","PATHOGEN")
dd <- do.call(rbind,cc)
head(dd)
dd[1,"PATHOGEN"]
cc[1]

sapply(ids, function(x) sapply(x,length))
ids[[279]]
