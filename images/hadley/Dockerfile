FROM ubuntu:12.04

RUN apt-get update
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8

RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y git libgmp10
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y cabal-install
RUN DEBIAN_FRONTEND=noninteractive apt-get install -q -y libpq-dev libssl-dev zlib1g-dev

RUN useradd -s /bin/bash -m gusdev

ADD hadley /usr/bin/hadley
# While Hadley is statically linked, it relies on HLint which has some
# data-files (normally present in .cabal).
ADD hlint-1.9.13 /home/gusdev/.cabal/share/hlint-1.9.13
RUN chown -R gusdev:gusdev /home/gusdev/.cabal

USER gusdev
WORKDIR /home/gusdev
ENV HOME /home/gusdev
ENV PATH /home/gusdev/.cabal/bin:$PATH

RUN cabal update
RUN cabal install alex happy

CMD hadley --help
