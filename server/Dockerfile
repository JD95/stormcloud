# Base Stack Image

FROM ubuntu 

CMD echo "Installing Git"

RUN apt-get update && apt-get install -y \
  git \
  pkg-config \
  libpq-dev \
  libsodium-dev \
  curl

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN ["git", "clone", "https://github.com/JD95/stormcloud"]

WORKDIR /stormcloud/server

COPY ["config.json", "/stormcloud/server/config.json"]

RUN ["stack", "setup"]

RUN ["stack", "build"]

CMD ["stack", "exec", "server-exe"]
