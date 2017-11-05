
# identifications that are considered contaminants and may be filtered by default
host_contaminants = c('Homo sapiens', 'Mus musculus', 'Chordata')
seq_contaminants = c(
  'artificial sequences',
  'Enterobacteria phage phiX174 sensu lato'
)
microbe_contaminants = c(
  'Enterobacteriales',
  'Cutibacterium acnes',
  'Escherichia coli',
  'Saccharomyces cerevisiae',
  'Ralstonia pickettii'
)
commoncontaminants <- c()  ## this vector is initially filtered

allcontaminants <-
  list(Host = host_contaminants,
       Artificial = seq_contaminants,
       Microbes = microbe_contaminants,
       Other = c('unclassified'))
#allcontaminants <- unlist(allcontaminants)
#names(allcontaminants) <- NULL
#allcontaminants <- sort(allcontaminants)
