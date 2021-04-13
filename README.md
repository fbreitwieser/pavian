Pavian is a interactive browser application for analyzing and visualization metagenomics classification results from classifiers such as 
Kraken, Centrifuge and MetaPhlAn. Pavian also provides an alignment viewer for validation of matches to a particular genome.

For more information look at the publication at https://doi.org/10.1093/bioinformatics/btz715

Pavian: interactive analysis of metagenomics data for microbiome studies and pathogen identification. FP Breitwieser, SL Salzberg - Bioinformatics, 2020

Thank you for citing the publication if Pavian helps in your research :).

You can try out Pavian at https://fbreitwieser.shinyapps.io/pavian/.

## Installation and deployment

Pavian is a R package, and thus requires R to run. Look [here](http://a-little-book-of-r-for-bioinformatics.readthedocs.io/en/latest/src/installr.html) for how to install R. On Windows, you probably need to install [Rtools](cran.r-project.org/bin/windows/Rtools/). On Ubuntu, install `r-base-dev`. Once you started R, the following commands will install the package:
```r
if (!require(remotes)) { install.packages("remotes") }
remotes::install_github("fbreitwieser/pavian")
```
To run Pavian from R, type
```r
pavian::runApp(port=5000)
```

Pavian will then be available at http://127.0.0.1:5000 in the web browser of you choice.

Alternatively, you can install and test Pavian with the following command:
```r
shiny::runGitHub("fbreitwieser/pavian", subdir = "inst/shinyapp")
```

To try out Pavian, load the [example files](https://github.com/fbreitwieser/pavian/tree/master/inst/shinyapp/example-data) directly from the interface.


# Installing Rsamtools

The alignment viewer uses [Rsamtools](https://bioconductor.org/packages/release/bioc/html/Rsamtools.html). To install this package from Bioconductor, use the following commands

```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("Rsamtools")
```

## Installing to Shinyapps.io

In order to install to Shinyapps.io, because of the Bioconductor repo dependencies, you need to first set the options using `setRepositories()` in R. At that point a `rsconnect::deployApp('pavian/inst/shinapp/') should work.


## Docker image

As an alternative to installing Pavian in R, a Docker image is available at [florianbw/pavian](https://hub.docker.com/r/florianbw/pavian/). When you run this docker image, Pavian will start automatically on port 80, which you need to make available to the hosting machine. On the shell, you can pull the image and remap the Docker port to port 5000 with the following commands:

```sh
docker pull 'florianbw/pavian'
docker run --rm -p 5000:80 florianbw/pavian
```

## Screenshots

![image](https://cloud.githubusercontent.com/assets/516060/20188595/5c8b9808-a747-11e6-9235-296a2314659a.png)

[![Build Status](https://travis-ci.org/fbreitwieser/pavian.svg?branch=master)](https://travis-ci.org/fbreitwieser/pavian)

## Supported formats

pavian natively supports the Kraken and MetaPhlAn-style report formats. In extension, you can use Centrifuge results by running `centrifuge-kreport` on Centrifuge output files, and Kaiju results by running `kraken-report` on Kaiju output files (see issue #11)

**Error: Maximum upload size exceeded**
The maximum upload size is defined by the option `shiny.maxRequestSize`. To increase it to 500 MB, for example, type the following before `pavian::runApp()`:
```
options(shiny.maxRequestSize=500*1024^2)
```
If your BAM file contains the unaligned reads, you can decrease the file size before uploading by getting rid of non-aligned reads using samtools view -F4.

# Acknowledgments

We'd like to thank the creators, contributors and maintainers of several packages without whom Pavian wouldn't exist:
 
 - Winston Chang, Hadley Wickham, Joe Cheng, JJ Allaire and all other developers at [Rstudio](https://shiny.rstudio.com/) and outside who contribute to the amazing set of packages behind shiny and the tidyverse (shiny, shinydashboard, DT, dplyr, plyr, htmltools, htmlwidgets, rmarkdown, knitr, ggplot2, rappdirs)
 - Mike Bostock and all developers behind the amazong [D3](https://d3js.org/) visualization library
 - Dean Atali for the [shinyjs](https://github.com/daattali/shinyjs) R package
 - dreamR developers for the [shinyWidgets](https://github.com/dreamRs/shinyWidgets) R package
 - Jonathan Owen for [rhandsontable](https://github.com/jrowen/rhandsontable) widget, based on the [handsontable](https://handsontable.com) javascript library
 - M. Morgan and the other developers behind [Rsamtools](https://bioconductor.org/packages/release/bioc/html/Rsamtools.html), as well as Heng Li and the other developers behind [samtools](https://github.com/samtools/samtools)
 - Christopher Garund and the other developers behind [networkD3](https://christophergandrud.github.io/networkD3/), on which sankeyD3 is based
 - The developers of [jstree](https://www.jstree.com/), on which shinyFileTree is based
