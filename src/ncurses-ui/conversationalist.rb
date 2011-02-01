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


$:.push('build/gen-rb')
$:.unshift('lib/thrift-rb')

require 'serialization.rb'

require 'thrift'
require 'scrabble_cheat'

require 'board.rb'


# Main class for communicating with the server. Handles communication protocols
class Conversationalist < ScrabbleCheat::Client

    def initialize
        port = 6655
        url = "http://127.0.0.1:#{port}"
        transport = Thrift::HTTPClientTransport.new(url)
        protocol = Thrift::BinaryProtocol.new(transport)
        super(protocol)
        transport.open()
    end


    def new_game(players)
        super(players)
    end

    def recv_new_game
        thrift_gamestate = super
        thrift_to_native_gamestate(thrift_gamestate)
    end

    def play_move(tiles, gamestate)
        thrift_tiles     = tiles.map { |x| native_to_thrift_tile(x) }
        thrift_gamestate = native_to_thrift_gamestate(gamestate)
        super(thrift_tiles, thrift_gamestate)
    end

    def recv_play_move
        thrift_gamestate = super 
        thrift_to_native_gamestate(thrift_gamestate)
    end
    
    def get_scrabblecheat_suggestions(rack, board)
        thrift_board = native_to_thrift_board(board)        
        super(rack, thrift_board)
    end

    def recv_get_scrabblecheat_suggestions
        thrift_movelist = super
        thrift_movelist.map do |move|
            movetiles = move.map { |x| thrift_to_native_tile(x) }
            {:move => movetiles, :score => move.score}
        end
    end

    # quit implemented in superclass.  Sends no args ^_^



private

    # 'native' tiles are simple hashes containing :letter_type, :letter, :bonus, :row, :col
    def native_to_thrift_tile(native_tile)
        tile        = ScrabbleCheat::Tile.new
        tile.row    = native_tile[:row]
        tile.col    = native_tile[:col]
        tile.type   = native_to_thrift_letter_type(native_tile[:letter_type])
        tile.letter = native_to_thrift_letter(native_tile[:letter])
        tile.bonus  = native_to_thrift_bonus(native_tile[:bonus])
        tile
    end


    def thrift_to_native_tile(thrift_tile)
        tile               = Hash.new 
        tile[:row]         = thrift_tile.row
        tile[:col]         = thrift_tile.col
        tile[:letter_type] = thrift_to_native_letter_type(thrift_tile.type)
        tile[:letter]      = thrift_to_native_letter(thrift_tile.letter)
        tile[:bonus]       = thrift_to_native_bonus(thrift_tile.bonus)
        tile
    end


    def thrift_to_native_gamestate(thrift_gamestate)
        gamestate = Hash.new
        gamestate[:board] = Board.from_list(thrift_gamestate.board.map { |x| thrift_to_native_tile(x) } )
        gamestate[:turn] = thrift_gamestate.player_turn
        gamestate[:scores] = thrift_to_native_scores(thrift_gamestate.scores, thrift_gamestate.turn_order)
        gamestate[:history] = thrift_to_native_history(thrift_gamestate.history)
        gamestate
    end


    def native_to_thrift_gamestate(native_gamestate)
        gamestate = ScrabbleCheat::Gamestate.new
        gamestate.player_turn = native_gamestate[:turn]
        gamestate.board       = native_gamestate[:board].to_thrift
        gamestate.turn_order  = native_gamestate[:scores].map { |x| x[0] }
        gamestate.scores      = native_gamestate[:scores].map { |x| {x[0] => x[1]} }
        gamestate.history     = native_to_thrift_history(native_gamestate[:history])
        gamestate
    end

    def thrift_to_native_scores(thrift_scores, turn_order)
        turn_order.map do |name|
            score = thrift_scores[name]
            [name, score]
        end
    end

    def native_to_thrift_history(native_history)
        native_history.map do |turn|
            name = turn.name
            movestruct = turn.move
            movetiles = movestruct.move.map { |x| thrift_to_native_tile(x) } 

            {:name => name, :move => movetiles, :score => movestruct.score}
        end
    end

    def thrift_to_native_history(thrift_history)
        thrift_history.map do |native_move|
            move = ScrabbleCheat::Move.new
            move.move   = native_move[:move]
            move.score  = native_move[:score]

            turn = ScrabbleCheat::Turn.new
            turn.player = native_move[:name]
            turn.move = move
        end
    end

    def native_to_thrift_letter_type(type)
        case type
            when :character
                ScrabbleCheat::LetterType::CHARACTER
            when :wildcard
                ScrabbleCheat::LetterType::WILDCARD
            when :none
                ScrabbleCheat::LetterType::EMPTY
        end
    end


    def thrift_to_native_letter_type(type)
        case type
            when ScrabbleCheat::LetterType::CHARACTER 
                :character
            when ScrabbleCheat::LetterType::WILDCARD  
                :wildcard
            when ScrabbleCheat::LetterType::EMPTY
                :none                
        end
    end


    def native_to_thrift_letter(letter)
        case letter 
            when :none
                ""
            else
                letter
        end
    end


    def thrift_to_native_letter(letter)
        case letter 
            when "" 
                :none
            else
                letter
        end
    end


    def native_to_thrift_bonus(bonus)
        case bonus
            when :triple_word_score
                ScrabbleCheat::Bonus::TRIPLE_WORD_SCORE
            when :double_word_score
                ScrabbleCheat::Bonus::DOUBLE_WORD_SCORE
            when :triple_letter_score
                ScrabbleCheat::Bonus::TRIPLE_LETTER_SCORE
            when :double_letter_score
                ScrabbleCheat::Bonus::DOUBLE_LETTER_SCORE
            when :none
                ScrabbleCheat::Bonus::NONE
        end
    end


    def thrift_to_native_bonus(bonus)
        case bonus
            when ScrabbleCheat::Bonus::TRIPLE_WORD_SCORE 
                :triple_word_score
            when ScrabbleCheat::Bonus::DOUBLE_WORD_SCORE
                :double_word_score
            when ScrabbleCheat::Bonus::TRIPLE_LETTER_SCORE
                :triple_letter_score
            when ScrabbleCheat::Bonus::DOUBLE_LETTER_SCORE
                :double_letter_score
            when ScrabbleCheat::Bonus::NONE
                :none
        end
    end




    # create_new_game :: [String] -> Gamestate
    #
    # Communicates to the server using the parametrized socket, and returns
    # the new board.
    def create_new_game_old(players)
        playerline = players.map { |x| x + "|" }.inject("") { |x,y| x + y }
        msg = "new_game&" + playerline
        response = polite_request(msg)

        Serialization.deserialize(:gamestate, response)
    end


    # get_scrabblecheat_moves :: Gamestate * String -> [Move]
    #
    # Queries the server for advice for how to best proceed.
    def get_scrabblecheat_moves_old(gamestate, rack)
        msg = "ai&" + Serialization.serialize(:gamestate, gamestate) + "&#{rack}"
        response = polite_request(msg)

        Serialization.deserialize(:movelist, response)
    end


     # play_move :: Gamestate * Move -> Gamestate
     #
     # Have the server play the move.  This keeps the game logic with the server.
     def play_move_old(gamestate, move)
         msg = "move&" + Serialization.serialize(:gamestate, gamestate) + "&" +
                         Serialization.serialize(:move, move)
         response = polite_request(msg)
 
         Serialization.deserialize(:gamestate, response)
    end

 
    # quit :: () -> ()
    #
    # Quits your interaction with the server.
    def quit_old()
         polite_request("quit")        
    end


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
