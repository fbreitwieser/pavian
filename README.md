Pavian is a web application for exploring metagenomics classification results, with a special focus on infectious disease diagnosis. Pinpointing pathogens in metagenomics classification results is often complicated by host and laboratory contaminants as well as many non-pathogenic microbiota. With Pavian, researchers can analyze, display and transform results from the Kraken and Centrifuge classifiers using
interactive tables, heatmaps and flow diagrams. Pavian also provides an alignment viewer for validation of matches to a particular genome.

## Installing and running Pavian

    # install.packages("devtools")
    devtools::install_github("fbreitwieser/pavian")
    pavian::runApp(port=5000)

With Docker [**there's an issue in the Docker image currently - will be fixed soon**]:

    docker pull 'florianbw/pavian'
    docker run --rm -p 5000:80 florianbw/pavian

In both cases Pavian will be available at http://127.0.0.1:5000 .
