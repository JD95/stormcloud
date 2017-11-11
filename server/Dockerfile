# Base Stack Image

FROM samdoshi/haskell-stack

CMD echo "Installing Git"

RUN apt-get update && apt-get install -y \
  git

RUN ["git", "clone", "https://github.com/JD95/stormcloud"]

WORKDIR /stormcloud/server

RUN ["stack", "setup"]

RUN ["stack", "build"]
