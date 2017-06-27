Pavian is a interactive browser application for analyzing and visualization metagenomics classification results from classifiers such as 
Kraken, Centrifuge and MetaPhlAn. Pavian also provides an alignment viewer for validation of matches to a particular genome.

For more information look at the preprint at http://biorxiv.org/content/early/2016/10/31/084715.full.pdf+html . Please cite the preprint if you use Pavian in your research.

**Installation and deployment**

Pavian is a R package, and thus requires R to run. Look [here](http://a-little-book-of-r-for-bioinformatics.readthedocs.io/en/latest/src/installr.html) for how to install R. Once you started R, the following commands will install the package:
```r
if (!require(devtools)) { install.packages("devtools") }
devtools::install_github("fbreitwieser/pavian")
```
To run Pavian from R, type
```r
pavian::runApp(port=5000)
```

Pavian will then be available at http://127.0.0.1:5000 in the web browser of you choice.

**Screenshots**

![image](https://cloud.githubusercontent.com/assets/516060/20188595/5c8b9808-a747-11e6-9235-296a2314659a.png)

[![Build Status](https://travis-ci.org/fbreitwieser/pavian.svg?branch=master)](https://travis-ci.org/fbreitwieser/pavian)
