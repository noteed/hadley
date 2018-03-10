FROM images.reesd.com/reesd/stack:7.10.3

ENV PATH /home/gusdev/.cabal/bin:$PATH
RUN cabal update
RUN cabal install pandoc
RUN cabal install hlint
