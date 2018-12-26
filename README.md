Pavian is a interactive browser application for analyzing and visualization metagenomics classification results from classifiers such as 
Kraken, Centrifuge and MetaPhlAn. Pavian also provides an alignment viewer for validation of matches to a particular genome.

For more information look at the preprint at http://biorxiv.org/content/early/2016/10/31/084715.full.pdf+html . Please cite the preprint if you use Pavian in your research.

You can try out Pavian at https://fbreitwieser.shinyapps.io/pavian/.

**Installation and deployment**

Pavian is a R package, and thus requires R to run. Look [here](http://a-little-book-of-r-for-bioinformatics.readthedocs.io/en/latest/src/installr.html) for how to install R. On Windows, you probably need to install [Rtools](cran.r-project.org/bin/windows/Rtools/). On Ubuntu, install `r-base-dev`. Once you started R, the following commands will install the package:
```r
options(repos = c(CRAN = "http://cran.rstudio.com"))
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


**Installing Rsamtools**

The alignment viewer uses `Rsamtools`. To install this package from Bioconductor, use the following commands

```r
source("https://bioconductor.org/biocLite.R")
biocLite("Rsamtools")
```

**Docker image**

As an alternative to installing Pavian in R, a Docker image is available at [florianbw/pavian](https://hub.docker.com/r/florianbw/pavian/). When you run this docker image, Pavian will start automatically on port 80, which you need to make available to the hosting machine. On the shell, you can pull the image and remap the Docker port to port 5000 with the following commands:

```sh
docker pull 'florianbw/pavian'
docker run --rm -p 5000:80 florianbw/pavian
```

**Screenshots**

![image](https://cloud.githubusercontent.com/assets/516060/20188595/5c8b9808-a747-11e6-9235-296a2314659a.png)

[![Build Status](https://travis-ci.org/fbreitwieser/pavian.svg?branch=master)](https://travis-ci.org/fbreitwieser/pavian)

**Supported formats**

pavian natively supports the Kraken and MetaPhlAn-style report formats. In extension, you can use Centrifuge results by running `centrifuge-kreport` on Centrifuge output files, and Kaiju results by running `kraken-report` on Kaiju output files (see issue #11)

**Error: Maximum upload size exceeded**
The maximum upload size is defined by the option `shiny.maxRequestSize`. To increase it to 500 MB, for example, type the following before `pavian::runApp()`:
```
options(shiny.maxRequestSize=500*1024^2)
```
If your BAM file contains the unaligned reads, you can decrease the file size before uploading by getting rid of non-aligned reads using samtools view -F4.


