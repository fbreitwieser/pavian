
options(warn=2)

install.packages(c("devtools", "htmlwidgets", "magrittr", "Rcpp",))
devtools::install_github("sankeyD3")

source("https://bioconductor.org/biocLite.R")
biocLite("Rsamtools")

install.packages(c("shiny","shinydashboard","shinyjs","shinyBS",
                   "sparkline","DT","rhandsontable",
                   "sankeyD3","scatterD3",
                   "ggplot2","htmltools",
                   "plyr","dplyr","yaml"))

devtools::install_github("fbreitwieser/pavian")

message("You successfully installed pavian v",packageVersion("pavian"),", congratulations !\n")
message("To start the web interface, run ")
message("  pavian::runApp(port=5000)")
