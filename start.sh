#!/usr/bin/env bash
set -e

docker build -t bios611 .

docker run --rm \
  -p 8787:8787 \
  -v "$PWD:/home/rstudio/work" \
  -e PASSWORD=dogsandcats \
  -it bios611