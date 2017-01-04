# Pavian - Interactive analysis of metagenomics data

Pavian is a web application for exploring metagenomics classification results, with a special focus on infectious disease diagnosis. Pinpointing pathogens in metagenomics classification results is often complicated by host and laboratory contaminants as well as many non-pathogenic microbiota. With Pavian, researchers can analyze, display and transform results from the Kraken and Centrifuge classifiers using interactive tables, heatmaps and flow diagrams. Pavian also provides an alignment viewer for validation of matches to a particular genome.

Source code and up-to-date documentation are available at https://github.com/fbreitwieser/pavian.

**Preprint now available at http://biorxiv.org/content/early/2016/10/31/084715.full.pdf+html**.

![image](https://cloud.githubusercontent.com/assets/516060/20188595/5c8b9808-a747-11e6-9235-296a2314659a.png)


## Installing and running Pavian

In R / RStudio:

    source("https://raw.githubusercontent.com/fbreitwieser/pavian/master/inst/shinyapp/install-pavian.R")
    pavian::runApp(port=5000)

With Docker:

    docker run --rm -p 5000:80 florianbw/pavian

In both cases Pavian will be available at http://127.0.0.1:5000 .

You can deploy this Docker image on your private server or on any <a href="https://docs.docker.com/machine/drivers/">cloud service</a>.

