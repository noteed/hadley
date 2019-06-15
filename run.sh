#! /usr/bin/env bash

mkdir -p _site
docker run \
  -v `pwd`:/home/gusdev/hadley \
  -v `pwd`/_site:/artifacts \
  -t -i images.reesd.com/reesd/stack-pandoc \
  sh -c \
    'cd hadley ;
    runghc -idist/build/autogen bin/hadley.hs clone https://github.com/noteed/hadley.git ;
    runghc -idist/build/autogen bin/hadley.hs generate --source /home/gusdev/.hadley/clone --target /artifacts ;
    '
