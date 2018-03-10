FROM images.reesd.com/reesd/stack:7.8.4

ENV PATH /home/gusdev/.cabal/bin:$PATH
RUN cabal update
#RUN cabal install alex happy
#RUN cabal install yaml-0.8.9 pandoc-1.12.3.3 scientific-0.3.3.2 hlint haddock-2.10.0
#RUN cabal install yaml pandoc scientific hlint haddock
#RUN cabal install pandoc

RUN cabal install pandoc-citeproc-0.7.4 --constraint=pandoc-types==1.12.4.7
# RUN cabal install hakyll
