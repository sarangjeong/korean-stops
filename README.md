# Korean-stop-contrast
This is my project on Korean stop contrast and the relation between production and perception under a sound change.

docker run --pull=missing --rm -ti \
  --name rstudio-dev \
  -p 127.0.0.1:8787:8787 \
  -e DISABLE_AUTH=true \
  -v "C:\Users\hanpe\github\korean-stops:/home/rstudio/project" \
  --log-driver json-file \
  --log-opt max-size=10m \
  --log-opt max-file=3 \
  rocker/rstudio:latest


docker run --pull=missing --rm -ti `
  --name rstudio-dev `
  -p 127.0.0.1:8787:8787 `
  -e DISABLE_AUTH=true `
  -v C:/Users/hanpe/github/korean-stops:/home/rstudio/project `
  --log-driver json-file `
  --log-opt max-size=10m `
  --log-opt max-file=3 `
  rocker/rstudio:latest
