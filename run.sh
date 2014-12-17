#! /usr/bin/env bash

mkdir -p _static
docker run \
  -v `pwd`:/home/gusdev/hadley \
  -v `pwd`/_static:/artifacts \
  -t -i images.reesd.com/reesd/stack-pandoc \
  sh -c \
    'cd hadley ;
    runghc -idist/build/autogen bin/hadley.hs clone https://github.com/noteed/hadley.git ;
    runghc -idist/build/autogen bin/hadley.hs generate --target /artifacts ;
    '
