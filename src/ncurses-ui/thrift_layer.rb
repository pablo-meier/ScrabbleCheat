# Copyright (c) 2010 Paul Meier # 
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

#
# Ugly, ugly hack while I figure out this Thrift stuff.
#

$:.push('build/gen-rb')
$:.unshift('lib/thrift-rb')

require 'thrift'
require 'scrabble_cheat'


class ThriftLayer

    ADDR  = '127.0.0.1'
    PORT = '8888'

    def initialize
        @transport = Thrift::BufferedTransport.new(Thrift::Socket.new(ADDR, PORT))
        protocol = Thrift::BinaryProtocol.new(@transport)
        @client = ScrabbleCheat::Client.new(protocol)
        @transport.open()
    end

    def new_game(playerlist)
        @client.new_game(playerlist)
    end

    def play_move(tiles, gamestate)
        @client.play_move(tiles, gamestate)
    end

    def get_scrabblecheat_suggestions(rack, board)
        @client.get_scrabblecheat_suggestions(rack, board)
    end

    def quit
        @transport.close()
        @client.quit()
    end
end

