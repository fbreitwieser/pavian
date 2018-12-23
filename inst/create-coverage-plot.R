suppressMessages(library(ggplot2))
#suppressMessages(library(cowplot))
suppressMessages(library(Rsamtools))

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("At least one argument must be supplied (BAM file).n", call.=FALSE)
}
bam_file <- args[1]
pileup <- pavian:::get_pileup(bam_file, min.mapq=10)
p <- pavian::plot_pileup(pileup) + facet_wrap(~seqnames)
ggsave(paste0(bam_file,".png"), p, width = 6, height = 2)
ggsave(paste0(bam_file,".pdf"), p, width = 6, height = 2)
ggsave(paste0(bam_file,".svg"), p, width = 6, height = 2)
