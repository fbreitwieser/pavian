
# identifications that are considered contaminants and may be filtered by default
host_contaminants = c('s_Homo sapiens')
seq_contaminants = c(
  's_synthetic construct',
  'u_unclassified',
  's_Enterobacteria phage phiX174 sensu lato'
)
microbe_contaminants = c(
  's_Propionibacterium acnes',
  's_Escherichia coli',
  's_Saccharomyces cerevisiae',
  's_Ralstonia pickettii'
)
commoncontaminants <- c()  ## this vector is initially filtered
allcontaminants <-
  list(Host = host_contaminants,
       Artificial = seq_contaminants,
       Microbes = microbe_contaminants)
allcontaminants <- unlist(allcontaminants)
names(allcontaminants) <- NULL
