# Base Stack Image

FROM stormcloud-server-deps:latest 

RUN ["git", "clone", "https://github.com/JD95/stormcloud"]

WORKDIR /stormcloud/server

COPY ["config.json", "/stormcloud/server/config.json"]

RUN ["stack", "build"]

CMD ["stack", "exec", "server-exe"]
