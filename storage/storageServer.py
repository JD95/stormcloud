import socket
from nacl.public import PrivateKey, Box
import base64
import nacl
import nacl.secret


with open("notKeys.bin", "rb") as b:
	f = b.read()
	key = f
	box = nacl.secret.SecretBox(key)
	#f = str(base64.b16encode(f))
	#f = f.replace("b'","").replace("'","")
	#print(f)

	m = box.encrypt(b"message")
	print(repr(m))
	print(box.decrypt(m))

def listen():
	connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	connection.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
	connection.bind(('0.0.0.0', 5555))
	connection.listen(10)
	while True:
		current_connection, address = connection.accept()
		print(address)
		while True:
			data = current_connection.recv(2048)
			print("got a message")
			if "random" in data:
				#print("'You' is trying to connect, do something.. or not, whatever.")
				#print(data.split("random")[0])
				message = b"SCHWIFTY"
				encrypted = box.encrypt(message)
				current_connection.send(encrypted)

			if True:
				print(data)
				data = base64.b16decode(data) 
				print(repr(data))
				#message = data.split("hello")
				#message.replace("\r\n\r\n","")
				text = box.decrypt(data)
				print(text)
				
				message = b"SCHWIFTY"
				encrypted = box.encrypt(message)
				print(encrypted)
				encrypted = base64.b16encode(encrypted)
				current_connection.send(encrypted+"\r\n\r\n")
				print("message sent")
				break
			


			'''
			if data == 'quit\r\n':
				current_connection.shutdown(1)
				current_connection.close()
				break

			elif data == 'stop\r\n':
				current_connection.shutdown(1)
				current_connection.close()
				exit()

			elif data:
				print("sending data back")
				current_connection.send(data)
				print("data: " +repr(data))
				print("done sending")
			'''

if __name__ == "__main__":
	try:
		listen()
	except KeyboardInterrupt:
		pass
