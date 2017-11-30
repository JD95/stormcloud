Invoke-Expression -Command (aws ecr get-login --no-include-email --region us-west-1)

# Build Server
cd server
docker build -t stormcloud_server .
docker tag stormcloud_server:latest 377531310647.dkr.ecr.us-west-1.amazonaws.com/stormcloud:latest

# Build Server Database
cd database
docker build -t stormcloud_auth_db .
docker tag stormcloud_auth_db:latest 377531310647.dkr.ecr.us-west-1.amazonaws.com/stormcloud:latest

# Push to aws repo
docker push 377531310647.dkr.ecr.us-west-1.amazonaws.com/stormcloud:latest
