# Ugly, ugly hack while I figure out this Thrift stuff.
$:.push('gen-rb')

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

    def new_game(playerlist, gamename, dict)
        @client.new_game(playerlist, gamename, dict)
    end

    def game_info(gamename)
        @client.game_info(gamename)
    end

    def play_move(tiles, gamestate)
        @client.play_move(tiles, gamestate)
    end

    def get_scrabblecheat_suggestions(rack, board, gamename, dict)
        @client.get_scrabblecheat_suggestions(rack, board, gamename, dict)
    end

    def quit
        @transport.close()
        @client.quit()
    end
end

