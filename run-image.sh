#! /usr/bin/env bash

# Try the Hadley Docker image.

mkdir -p _static
docker run \
  -v `pwd`/_static:/artifacts \
  -t -i noteed/hadley \
  sh -c \
    'hadley clone https://github.com/noteed/hadley.git ;
     cd /home/gusdev/.hadley/clone ;
     hadley generate --target /artifacts ;
  '
