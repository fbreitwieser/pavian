FROM rocker/r-ver:3.5.3

MAINTAINER Florian Breitwieser "florian.bw@gmail.com"

## based on https://hub.docker.com/r/rocker/shiny/~/dockerfile/ by Winston Chang

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget

# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    chown shiny:shiny /var/lib/shiny-server

RUN R -e "options(repos = c(CRAN = 'http://cran.rstudio.com')); install.packages(c('remotes'));"
RUN R -e "source('https://bioconductor.org/biocLite.R'); biocLite('Rsamtools')"
RUN R -e "remotes::install_github('fbreitwieser/pavian')"

COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
RUN rm -rf /srv/shiny-server/*
RUN R -e "sapply(list.files(system.file('shinyapp',package='pavian'),full.names=TRUE),file.copy,to='/srv/shiny-server/',recursive=TRUE)"

EXPOSE 80

COPY shiny-server.sh /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]
