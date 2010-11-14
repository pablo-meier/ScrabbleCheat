require 'socket'
include Socket::Constants

PORT = 6655                     # Hard coding for now, will generalize later.


# create_new_game :: Socket -> Gamestate
#
# Communicates to the server using the parametrized socket, and returns
# the new board.
def create_new_game(sock)
    sock.write("new_game")
    puts "CLIENT: new_game sent..."

    gamestate_len = sock.recv(1024).strip.to_i
    sock.write("thanks, ready")
    gamestate_string = sock.recv(gamestate_len)
    Serialization.deserialize(:gamestate, gamestate_string)
end




socket = TCPSocket.new("localhost", 6655)
puts "CLIENT: created socket..."
gamestate = create_new_game(socket)



puts "We got #{board.to_s}"
Process.exit
