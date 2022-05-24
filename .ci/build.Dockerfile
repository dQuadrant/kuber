ARG UBUNTU_VERSION=20.04
FROM ubuntu:${UBUNTU_VERSION}
ENV DEBIAN_FRONTEND=nonintercative
RUN mkdir -p /app/src
WORKDIR /app

# development dependencies
RUN apt-get update -y && apt-get install -y \
  automake \
  build-essential \
  g++\
  git \
  jq \
  libffi-dev \
  libghc-postgresql-libpq-dev \
  libgmp-dev \
  libncursesw5 \
  libpq-dev \
  libssl-dev \
  libsystemd-dev \
  libtinfo-dev \
  libtool \
  make \
  pkg-config \
  tmux \
  wget \
  zlib1g-dev libreadline-dev llvm libnuma-dev \
  && rm -rf /var/lib/apt/lists/*

ARG CABAL_VERSION=3.6.2.0
ARG GHC_VERSION=8.10.7
ARG IOHK_LIBSODIUM_GIT_REV=66f017f16633f2060db25e17c170c2afa0f2a8a1

# install secp2561k library with prefix '/'
RUN git clone https://github.com/bitcoin-core/secp256k1 &&\
  cd secp256k1 \
  && git fetch --all --tags &&\
  git checkout master \
  && ./autogen.sh && \
  ./configure --prefix= --enable-module-schnorrsig --enable-experimental && \
  make && \
  make install  && cd .. && rm -rf ./secp256k1


# install libsodium from sources with prefix '/'
RUN git clone https://github.com/input-output-hk/libsodium.git &&\
  cd libsodium \
  && git fetch --all --tags &&\
  git checkout ${IOHK_LIBSODIUM_GIT_REV} \
  && ./autogen.sh && \
  ./configure --prefix= && \
  make && \
  make install  && cd .. && rm -rf ./libsodium

# install cabal
RUN wget --secure-protocol=TLSv1_2 \
  "https://downloads.haskell.org/~cabal/cabal-install-${CABAL_VERSION}/cabal-install-${CABAL_VERSION}-$(arch)-linux-deb10.tar.xz" && \
  tar -xf *.tar.xz &&\
  rm *.tar.xz &&\
  mv cabal /usr/local/bin/

# install ghc from sources
WORKDIR /app/ghc
RUN wget --secure-protocol=TLSv1_2 \
  "https://downloads.haskell.org/~ghc/${GHC_VERSION}/ghc-${GHC_VERSION}-$(arch)-deb10-linux.tar.xz" &&\
  tar -xf *.tar.xz &&\
  rm *.tar.xz \
  && cd /app/ghc/ghc-${GHC_VERSION} \
  && ./configure && make install \
  && cd .. && rm -rf /app/ghc/ghc-${GHC_VERSION}

WORKDIR /app/src