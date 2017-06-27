Pavian is a interactive browser application for analyzing and visualization metagenomics classification results from classifiers such as 
Kraken, Centrifuge and MetaPhlAn. Pavian also provides an alignment viewer for validation of matches to a particular genome.

**For more information look at the Preprint at http://biorxiv.org/content/early/2016/10/31/084715.full.pdf+html**

![image](https://cloud.githubusercontent.com/assets/516060/20188595/5c8b9808-a747-11e6-9235-296a2314659a.png)

** Installation and deployment

In R / RStudio:

    ## Installs required packages from CRAN and Bioconductor
    if (!require(devtools)) { install.packages("devtools") }
    devtools::install_github("fbreitwieser/pavian")
    pavian::runApp(port=5000)

With Docker:

    docker pull 'florianbw/pavian'
    docker run --rm -p 5000:80 florianbw/pavian

In both cases Pavian will be available at http://127.0.0.1:5000 .

[![Build Status](https://travis-ci.org/fbreitwieser/pavian.svg?branch=master)](https://travis-ci.org/fbreitwieser/pavian)
