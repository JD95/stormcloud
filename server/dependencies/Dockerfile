# Base Stack Image

FROM ubuntu:17.10

CMD echo "Installing Git"

RUN apt-get update && apt-get install -y \
  git \
  pkg-config \
  libpq-dev \
  libsodium-dev \
  curl

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN  stack --resolver lts-9.14 setup

RUN stack --resolver lts-9.14 install \
  Spock \
  aeson \
  base16-bytestring \
  bytestring \
  containers \
  cryptohash \
  cryptonite \
  directory \
  hvect \
  lens-aeson \
  memory \
  microlens-platform \
  monad-logger \
  monad-loops \
  mtl \
  network-simple \
  optparse-applicative \
  persistent \
  persistent-postgresql \
  persistent-template \
  process \
  protolude \
  random \
  saltine \
  stm \
  text \
  time \
  time-units \
  wai \
  wai-cors \
  wai-middleware-static \
  warp \
  warp-tls \
  wreq
