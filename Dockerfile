FROM haskell:7.8

RUN cabal update && cabal install cabal-install

ENV PATH .cabal-sandbox/bin:$HOME/.cabal/bin:$PATH
