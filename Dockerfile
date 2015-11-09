FROM haskell

RUN apt-get update && apt-get install -y git
RUN git clone https://github.com/sgillis/hdevtools.git
WORKDIR /hdevtools
RUN cabal update && cabal install -j

ADD . /apiconf
WORKDIR /apiconf

RUN cabal update
RUN cabal install --dependencies-only --enable-tests -j
RUN cabal configure --enable-tests
RUN cabal build -j
RUN cabal install -j
