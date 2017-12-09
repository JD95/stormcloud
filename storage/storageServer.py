import socket
from nacl.public import PrivateKey, Box
import base64
import nacl
import nacl.secret
import os 

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
			data = repr(current_connection.recv(2048))
			print("got a message", data)
	
			if "store" in data:
				print("store")
				self.store(data, current_connection)

			if "retrieve" in data: 
				print("retrieve")
				self.retrieve(data, current_connection)

			if "delete" in data:
				print("delete")
				self.delete(data, current_connection)

				
	def store(self, packet, connection): 
		## format: 'store\r\n' <file name hash> '\r\n' <encrypted file data> '\r\n\r\n'
		self.logString += "s"
		packet = packet.split("\\r\\n")
		print(packet)
		payload = repr(self.box.decrypt(base64.b16decode(packet[1])))
		payload = payload.split("\\r\\n")
		print(payload)
		
		with open("files/" + payload[0].replace("b'", "") + ".bin", 'wb') as f:
			f.write(base64.b16decode(payload[1].replace("'", "")))
		
		self.respond(connection, "store")		

	def retrieve(self, packet, connection): 
		## format: 'retrieve\r\n' <file name hash> '\r\n\r\n'
		self.logString += "r"
		packet = packet.split("\\r\\n")
		print(packet)

		filename = packet[1].replace("'", "")
		print(filename)
		print("filename: " + str(self.box.decrypt(base64.b16decode(filename)).decode('ascii')))	
		with open("files/" + str(self.box.decrypt(base64.b16decode(filename)).decode('ascii')) + ".bin", 'rb') as f:
			payload = base64.b16encode(f.read())
			print("read data: " + str(payload)) 
		
		self.respond(connection, "retrieve", payload=payload)


	def delete(self, packet, connection): 
		## delete: 'delete\r\n' <file name hash> '\r\n\r\n'
		self.logString += "d"
		packet = packet.split("\\r\\n")
		print("Transmitting to NS---- DELETING" + str(packet[1]))
		os.remove("files/" + str(packet[1]) + ".bin")	
		self.respond(connection, "delete")


	def respond(self, connection, commandType, payload=b""):
		#response = str(commandType) + "\r\n"
		response = ""
		if commandType == "retrieve":
			print("appending return payload")
			payload += b"\r\n"
			print(repr(payload))
			
		response += self.logString[-50:] #+ "\r\n\r\n"
		print("return message: " + str(response))
	
		response = payload + bytes(response, 'utf-8')	
		encrypted = self.box.encrypt(response)
		encrypted = base64.b16encode(encrypted)
		connection.send(bytes(commandType, 'utf-8') + b"\r\n" + encrypted + b"\r\n\r\n")

 
x = storageServer()
x.listen()
