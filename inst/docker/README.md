Dockerized Shiny App
=======================

This is the Dockerized Shiny App [Wordcloud](http://shiny.rstudio.com/gallery/word-cloud.html)

This Dockerfile is based on Debian "testing" and r-base image.

The image is available from [Docker Hub](https://registry.hub.docker.com/u/fbreitwieser/pavian/).

## Usage:

To run this Shiny App on your computer:

```sh
docker run --rm -p 80:80 fbreitwieser/pavian
```

and it will avaliable at http://127.0.0.1/ ou http://localhost

You can run the container at other ports. Sometimes there is already some services running at PORT 80, as Apache ou Nginx.
To run the app at PORT 3838 for example, you can use:

```sh
docker run --rm -p 3838:80 fbreitwieser/pavian
```

## Intented usage:

This project can be used as a start point to build any dockerized shiny app that could be distributed at any server running docker.
Possible use cases are:

* Deploy a single Shiny App at AWS, Google Compute Engine, Azure or a private server with docker.
* Deploy Shiny Apps at a docker based PaaS as [dokku](https://github.com/progrium/dokku). 

## Deploy with a docker based PaaS

If you have a PaaS with Dockerfiles support, like [Deis](http://deis.io/) or [Dokku](https://github.com/progrium/dokku), you can git push this image. I just wrote a post with further instructions: [Git pushing Shiny Apps with docker and dokku](http://www.fbreitwieser.net/2015/05/11/git-pushing-shiny-apps-with-docker-dokku/)

