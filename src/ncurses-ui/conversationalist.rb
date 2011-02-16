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

require 'thrift'
require 'scrabble_cheat'

require 'thrift_layer.rb'
require 'board.rb'


# Main class for communicating with the server. Handles communication protocols
class Conversationalist 

    def initialize
        @thrift = ThriftLayer.new
    end

    def new_game(namelist)
        rslt = @thrift.new_game(namelist)
        parse_new_game(rslt)
    end

    def parse_new_game(thrift_gamestate)
        thrift_to_native_gamestate(thrift_gamestate)
    end


    def play_move(gamestate, tiles)
        thrift_tiles     = tiles.map { |x| native_to_thrift_tile(x) }
        thrift_gamestate = native_to_thrift_gamestate(gamestate)
        rslt = @thrift.play_move(thrift_tiles, thrift_gamestate)
        parse_play_move(rslt)
    end

    def parse_play_move(thrift_gamestate)
        thrift_to_native_gamestate(thrift_gamestate)
    end


    
    def get_scrabblecheat_suggestions(rack, board)
        thrift_board = board.to_list.map { |x| native_to_thrift_tile(x) }
        rslt = @thrift.get_scrabblecheat_suggestions(rack, thrift_board)
        parse_get_scrabblecheat_suggestions(rslt)
    end

    def parse_get_scrabblecheat_suggestions(thrift_movelist)
        thrift_movelist.map do |move_obj|
            movetiles = move_obj.move.map { |x| thrift_to_native_tile(x) }
            {:move => movetiles, :score => move_obj.score}
        end
    end

    def quit
        @thrift.quit
    end



private

    # 'native' tiles are simple hashes containing :letter_type, :letter, :bonus, :row, :col
    def native_to_thrift_tile(native_tile)
        tile        = Tile.new
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
        gamestate[:board]   = Board.from_list(thrift_gamestate.board.map { |x| thrift_to_native_tile(x) } )
        gamestate[:turn]    = thrift_gamestate.player_turn
        gamestate[:scores]  = thrift_to_native_scores(thrift_gamestate.scores, thrift_gamestate.turn_order)
        gamestate[:history] = thrift_to_native_history(thrift_gamestate.history)
        gamestate
    end


    def native_to_thrift_gamestate(native_gamestate)
        gamestate = Gamestate.new
        gamestate.player_turn = native_gamestate[:turn]
        gamestate.board       = native_gamestate[:board].to_list.map { |x| native_to_thrift_tile(x) }
        gamestate.turn_order  = native_gamestate[:scores].map { |x| x[0] }
        gamestate.history     = native_to_thrift_history(native_gamestate[:history])

        gamestate.scores      = Hash.new
        native_gamestate[:scores].each do |pair| 
            gamestate.scores[ pair[0] ] = pair[1]
        end
        gamestate
    end


    def thrift_to_native_scores(thrift_scores, turn_order)
        turn_order.map do |name|
            score = thrift_scores[name]
            [name, score]
        end
    end


    def native_to_thrift_history(native_history)
        native_history.map do |native_turn|
            move        = Move.new
            move.move   = native_turn[:move].map { |x| native_to_thrift_tile(x) }
            move.score  = native_turn[:score]

            turn        = Turn.new
            turn.player = native_turn[:name]
            turn.move   = move

            turn
        end
    end


    def thrift_to_native_history(thrift_history)
        thrift_history.map do |turn|
            native_move         = Hash.new
            native_move[:move]  = turn.move.move.map { |x| thrift_to_native_tile(x) }
            native_move[:score] = turn.move.score
            native_move[:name]  = turn.player

            native_move
        end
    end


    def native_to_thrift_letter_type(type)
        case type
            when :character
                LetterType::CHARACTER
            when :wildcard
                LetterType::WILDCARD
            when :none
                LetterType::EMPTY
        end
    end


    def thrift_to_native_letter_type(type)
        case type
            when LetterType::CHARACTER 
                :character
            when LetterType::WILDCARD  
                :wildcard
            when LetterType::EMPTY
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
                Bonus::TRIPLE_WORD_SCORE
            when :double_word_score
                Bonus::DOUBLE_WORD_SCORE
            when :triple_letter_score
                Bonus::TRIPLE_LETTER_SCORE
            when :double_letter_score
                Bonus::DOUBLE_LETTER_SCORE
            when :none
                Bonus::NONE
        end
    end

    def thrift_to_native_bonus(bonus)
        case bonus
            when Bonus::TRIPLE_WORD_SCORE 
                :triple_word_score
            when Bonus::DOUBLE_WORD_SCORE
                :double_word_score
            when Bonus::TRIPLE_LETTER_SCORE
                :triple_letter_score
            when Bonus::DOUBLE_LETTER_SCORE
                :double_letter_score
            when Bonus::NONE
                :none
        end
    end

end
