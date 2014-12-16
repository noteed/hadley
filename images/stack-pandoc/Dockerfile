FROM images.reesd.com/reesd/stack

ENV PATH /home/gusdev/.cabal/bin:$PATH
RUN cabal update
RUN cabal install alex happy
RUN cabal install yaml-0.8.9 pandoc-1.12.3.3 scientific-0.3.3.2 hlint haddock-2.10.0
