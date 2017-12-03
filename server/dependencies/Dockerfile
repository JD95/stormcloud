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
  saltine
