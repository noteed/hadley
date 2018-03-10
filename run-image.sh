#! /usr/bin/env bash

# Try the Hadley Docker image.

mkdir -p _static
docker run \
  -it \
  -v $(pwd)/_static:/artifacts \
  noteed/hadley \
  sh -c \
    'hadley clone https://github.com/noteed/hadley.git ;
     cd /home/gusdev/.hadley/clone ;
     hadley generate --target /artifacts ;
  '
