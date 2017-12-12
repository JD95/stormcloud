# stormcloud

## Storage Server Setup
The storage server is implemented in python and must be run with Python3.
1. Install Pynacl. The remaining python packages base64, os and socket
come built into most Python3 interpreters. If they are not you can install
each of the using Pip. 

2. Run 'Python storageServer.py' in the command line.

	Note: The storage server must be active before the authentication server
makes its first request.
 
## Auth Server Setup

1. Install libpq-dev, libsodium-dev, and pkg-config

> apt-get update && apt-get install -y pkg-config libpq-dev libsodium-dev

Note that the needed version of libsodium-dev might not be available from the default repositories. The build has been tested in ubuntu-17.10 but not earlier versions.

2. Install Haskell package manager Stack via their install script

> curl -sSL https://get.haskellstack.org/ | sh

3. Using Stack, install the appropriate Haskell compiler

> cd server
> stack setup

4. Install dependencies for project and build

> stack build

5. Create a configuration file based on this json template

```json
{
    "dbConnStr": "...",
    "ip": "127.0.0.1",
    "port": 5555,
    "secretKey": "D0E71F4118793D91894176C1BE7100EC85F34EC8DB25AD31FF69955E413CBAEE"
}
```

`dbConnStr` is the database connection string. `ip` and `port` are for the storage server. The `secretKey` is the shared key between the authentication server and the storage server, a new key can be generated, but the one above is used for testing.

5. Launch the server

> stack exec server-exe

## Client Setup
