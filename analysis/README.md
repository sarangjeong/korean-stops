This folder contains R scripts for analysis, graphs, and raw data that are going to be processed using R.

# Development with Docker

# Run container (removes after exit)
docker run -ti -e DISABLE_AUTH=true -p 127.0.0.1:8787:8787 -v ${PWD}:/home/rstudio/work rocker/rstudio