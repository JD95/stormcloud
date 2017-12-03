import socket
from nacl.public import PrivateKey, Box
private = PrivateKey.generate()
public = private.public_key


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
			print(data)
			if "random:" in data:
				print("'You' is trying to connect, do something.. or not, whatever.")
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
