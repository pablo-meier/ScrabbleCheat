# Copyright (c) 2010 Paul Meier
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

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
