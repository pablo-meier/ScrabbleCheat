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


require 'conversationalist.rb'

# Not really test cases, but this was the boilerplate code I used when testing
# this manually.  Storing here while I continue hacking;  maybe someday they'll
# become real, automated tests!


socket = TCPSocket.new("localhost", 6655)
client = Conversationalist.new(socket)
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
movelist.each do |movepair|
    move = movepair[:move]
    score = movepair[:score]
    move.each do |tile|
        print "(#{tile[:row]}, #{tile[:col]}) -> #{tile[:letter]}, "
    end
    print "$ WORTH #{score.to_s}\n  "
end
puts "press a character to quit."
char = gets.chomp

client.quit


