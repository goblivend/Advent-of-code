FROM haskell:9.6.6

# Install best text editor
RUN apt-get update && apt-get install -y vim

RUN cabal update

# Copy base project to force cabal to install dependencies in the image
COPY ./haskell /home/haskell

WORKDIR /home/haskell

# Installing the program's dependencies
RUN cabal build

# Installing the profiling dependencies
RUN cabal build --enable-profiling

RUN echo '#!/bin/sh\n ./dist-newstyle/build/x86_64-linux/ghc-9.6.6/Main-0.1.0.0/x/Main/build/Main/Main +RTS -p -RTS $@' > profile && chmod u+x profile
RUN echo '#!/bin/sh\n ./dist-newstyle/build/x86_64-linux/ghc-9.6.6/Main-0.1.0.0/x/Main/build/Main/Main $@' > run && chmod u+x run

CMD ["bash"]
