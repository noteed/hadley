FROM ubuntu:12.04

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8

RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y git libgmp10

RUN useradd -s /bin/bash -m gusdev

USER gusdev
WORKDIR /home/gusdev
ENV HOME /home/gusdev

ADD hadley /usr/bin/hadley
# While Hadley is statically linked, it relies on HLint which has some
# data-files (normally present in .cabal).
ADD hlint-1.9.13 /home/gusdev/.cabal/share/hlint-1.9.13

CMD hadley --help
