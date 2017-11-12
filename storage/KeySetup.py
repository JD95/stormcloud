# Cameron Franke 
import nacl
import nacl.utils
import nacl.secret
from nacl.public import PrivateKey, Box
import base64
import binascii

private = PrivateKey.generate()
public = private.public_key

key = nacl.utils.random(nacl.secret.SecretBox.KEY_SIZE)
print(type(key))
print(base64.b16encode(key))


pubk = base64.b16encode(public.__bytes__()).decode('ascii')
print("my public key: " + pubk)
prik = base64.b16encode(private.__bytes__()).decode('ascii')
print("my private key: " + prik)

#with open("notKeys.bin", "wb") as b:
#	b.write(public.__bytes__())
#	b.write(private.__bytes__())

with open("notKeys.bin", "wb") as b:
	b.write(key)

#with open("notKeys.bin", "rb") as b:
#	f = b.read()
#	f = str(base64.b16encode(f))
#	f = f.replace("b'","").replace("'","")
#	print("read public key: " + f[:int(len(f)/2)])
#	print("read private key: " + f[int(len(f)/2):])

