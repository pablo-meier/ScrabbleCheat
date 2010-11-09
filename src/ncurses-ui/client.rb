require 'socket'
include Socket::Constants

PORT = 6655   # Hard coding for now, will generalize later.

# On activation, send "new_game" to erlang server.

socket = TCPSocket.new("localhost", 6655)
puts "CLIENT: created socket..."
socket.write("new_game")
puts "CLIENT: new_game sent..."

output = socket.recv(1024)

puts "We got #{output}"
Process.exit
