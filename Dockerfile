FROM haskell

ADD . /apiconf
WORKDIR /apiconf

RUN cabal update && cabal install -j
