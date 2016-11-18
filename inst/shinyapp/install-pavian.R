
options(warn=2)
options(repos = c(CRAN = "http://cran.rstudio.com"))

if (!require(Rsamtools)) {
  message("Installing Rsamtools from Bioconductor")
  source("https://bioconductor.org/biocLite.R")
  biocLite("Rsamtools")
}

all_packages <- c("devtools", "htmlwidgets", "magrittr", "Rcpp","shiny",
                  "shinydashboard","shinyjs","shinyBS","sparkline","DT",
                  "rhandsontable","sankeyD3","scatterD3","ggplot2",
                  "htmltools","plyr","dplyr","yaml")

packages_to_install <- all_packages[!all_packages %in% rownames(installed.packages())]

if (length(packages_to_install) > 0) {
  message("Installing ",length(packages_to_install), " required packages")
  install.packages(packages_to_install)
}

message("Installing sankeyD3 from https://github.com/fbreitwieser/sankeyD3")
devtools::install_github("fbreitwieser/sankeyD3")

message("Installing pavian from https://github.com/fbreitwieser/pavian")
devtools::install_github("fbreitwieser/pavian")

message("You successfully installed pavian v",packageVersion("pavian"),", congratulations !\n")
message("To start the web interface, run ")
message("  pavian::runApp(port=5000)")
