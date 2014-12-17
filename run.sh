#! /usr/bin/env bash

docker run \
  -v `pwd`:/home/gusdev/hadley \
  -t -i images.reesd.com/reesd/stack-pandoc \
  sh -c \
    'cd hadley ;
    runghc -idist/build/autogen bin/hadley.hs clone https://github.com/noteed/hadley.git ;
    runghc -idist/build/autogen bin/hadley.hs generate --refresh 5 ;
    '
