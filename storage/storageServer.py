import socket
from nacl.public import PrivateKey, Box
import base64
import nacl
import nacl.secret

class storageServer: 

	def __init__(self):
		#read in secret key from file 
		with open("notKeys.bin", "rb") as b:
			f = b.read()
			key = f
			self.box = nacl.secret.SecretBox(key)
		
			self.connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        	self.connection.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        	self.connection.bind(('0.0.0.0', 5555))
			
			self.logString = ""


	def listen(self):
		self.connection.listen(10)#what does the 10 mean here? 
		while True:
			current_connection, address = self.connection.accept()
			print("MAKE SURE TO VERIFY THAT THE MESAGE IS FROM THE EXPECTED IP"+ str(address))
			while True:
				data = current_connection.recv(2048)
				print("got a message")
	
				if "store" in data:
					print("Stub: implement store")
					self.store(data)

				if "retrieve" in data: 
					print("Stub: implement retrieve")
					self.retrieve(data)

				if "delete" in data:
					print("Stub: implement delete")
					self.delete(data)


				################################## sample send/ recieve ###############
				if "random" in data:
					message = b"SCHWIFTY"
					encrypted = box.encrypt(message)
					current_connection.send(encrypted)


				if True:
					print("raw message: " + repr(data) + "\nend of message")
					data = data.split("\r\n")[1]
					data = base64.b16decode(data) 
					print(repr(data))
					text = box.decrypt(data)
					print("plaintext: " + str(text))
				
					message =  b"SCHWIFTY"
					encrypted = box.encrypt(message)
					#print(encrypted)
					encrypted = base64.b16encode(encrypted)
					current_connection.send(b"test\r\n" + encrypted + b"\r\n\r\n")
					print("encrypted message sent")
					break
				####################################
				
	def store(self, packet): 
		## format: 'store\r\n' <file name hash> '\r\n' <encrypted file data> '\r\n\r\n'
		self.logString += "s"

	def retrieve(self, packet): 
		## format: 'retrieve\r\n' <file name hash> '\r\n\r\n'
		self.logString += "r"

	def delete(self, packet): 
		## delete: 'delete\r\n' <file name hash> '\r\n\r\n'
		self.logString += "d"

	def respond(self, commandType):
		response = str(commandType) + "\r\n"
		if commandType == "retrieve":
			print("append return payload")
		response += self.logString[-50:] + "\r\n\r\n"
		print("return message: " + str(response))
		print("NEED TO IMPLEMENT SENDING OF RESPONSE STRING")

		
		


 
x = storageServer()
x.listen()
