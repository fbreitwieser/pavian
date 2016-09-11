
library(phyloseq)
data(GlobalPatterns)
x <- GlobalPatterns

library(metagenomeSeq)
data(mouseData)
y <- mouseData

counts_table(x)[1:5,1:5]
counts_table(y)[1:5,1:5]
counts_table(y)[,1:5]

sample_data(x)[1:5,1:5]
sample_data(y)[1:5,1:5]

otu_tax_table(x)[1:5,]
otu_tax_table(y)[1:5,]

tax_tree(otu_tax_table(x))

## Should I extend the phyloseq or metagenomeSeq class?



