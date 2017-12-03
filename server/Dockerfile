# Base Stack Image

FROM samdoshi/haskell-stack

CMD echo "Installing Git"

RUN apt-get update && apt-get install -y \
  git \
  pkg-config \
  libpq-dev \
  libsodium-dev

RUN ["git", "clone", "https://github.com/JD95/stormcloud"]

WORKDIR /stormcloud/server

COPY ["config.json", "/stormcloud/server/config.json"]

RUN ["stack", "setup"]

RUN ["stack", "build"]

CMD ["stack", "exec", "server-exe"]
