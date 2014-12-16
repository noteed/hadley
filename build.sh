#! /usr/bin/env bash

docker run \
  -v `pwd`:/home/gusdev/hadley \
  -t -i images.reesd.com/reesd/stack-pandoc \
  sh -c 'cd hadley ; cabal install'
