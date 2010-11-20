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
require 'serialization.rb'
include Socket::Constants

PORT = 6655                     # Hard coding for now, will generalize later.



# Main class for communicating with the server.  Handles game flow, and basic 
# communication protocols
class Client

    def initialize(socket)
        @socket = socket
    end


    # create_new_game :: Socket * [String] -> Gamestate
    #
    # Communicates to the server using the parametrized socket, and returns
    # the new board.
    def create_new_game(players)
        playerline = players.map { |x| x + "|" }.inject("") { |x,y| x + y }
        msg = "new_game&" + playerline
        response = polite_request(msg)

        Serialization.deserialize(:gamestate, response)
    end


    # get_scrabblecheat_moves :: Gamestate * String -> [Move]
    #
    # Queries the server for advice for how to best proceed.
    def get_scrabblecheat_moves(gamestate, rack)
        msg = "ai&" + Serialization.serialize(:gamestate, gamestate) + "&#{rack}"
        response = polite_request(msg)

        Serialization.deserialize(:movelist, response)
    end


    # play_move :: Gamestate * Move -> Gamestate
    #
    # Have the server play the move.  This keeps the game logic with the server.
    def play_move(gamestate, move)
        msg = "move&" + Serialization.serialize(:gamestate, gamestate) + "&" +
                        Serialization.serialize(:move, move)
        response = polite_request(msg)

        Serialization.deserialize(:gamestate, response)
   end


   # quit :: () -> ()
   #
   # Quits your interaction with the server.
   def quit()
        polite_request("quit")        
   end

private

    # polite_request :: String -> String
    #
    # Follows the main:polite_response protocol of the server, where we communicate
    # how many bytes we're sending and receiving so we know how many to read.
    def polite_request(msg)
        @socket.write(msg.length.to_s)
        if (reply = @socket.recv(1024).strip) != "thanks" 
            puts "when communicating message of #{msg.length.to_s} length, got back #{reply}"
            return
        end
        @socket.write(msg)
        puts "Sent Message:\n  #{msg}"
        length = @socket.recv(1024).strip.to_i
        puts "Told to receive #{length} bytes"
        @socket.write("thanks")
        response = @socket.recv(length)
        puts "response was:\n  #{response}"
        response
    end
end


socket = TCPSocket.new("localhost", 6655)
client = Client.new(socket)
puts "CLIENT: created client with socket..."
gamestate = client.create_new_game(["Paul", "Sam"])
puts "CLIENT: Got fresh gamestate:\n  #{gamestate}"

puts "\nType a character to continue..."
char = gets.chomp

move = [{:letter_type => :character, :letter => "A", :bonus => :double_letter_score, :row => 7, :col => 7},
        {:letter_type => :character, :letter => "B", :bonus => :none,                :row => 7, :col => 8},
        {:letter_type => :character, :letter => "L", :bonus => :double_letter_score, :row => 7, :col => 9},
        {:letter_type => :character, :letter => "E", :bonus => :none,                :row => 7, :col => 10}]

new_gamestate = client.play_move(gamestate, move)

puts "CLIENT: sent move ABLE, got:\n  #{new_gamestate}"
puts "Type a character to continue..."
char = gets.chomp

movelist = client.get_scrabblecheat_moves(new_gamestate, "ZYGOTE")
puts "CLIENT: asked for help, got:\n  "
movelist.each do |move|
    move.each do |tile|
        print "(#{tile[:row]}, #{tile[:col]}) -> #{tile[:letter]}, "
    end
    print "\n  "
end
puts "press a character to quit."
char = gets.chomp

client.quit

Process.exit
