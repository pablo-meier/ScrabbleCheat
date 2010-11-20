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

require 'board.rb'

# Class with static methods for serialization and deserialization of the 
# various datatypes that make up a gamestate.
#
# I've never felt dirtier than I have writing this atrocious code.
class Serialization

    # deserialize :: Str -> Object
    #
    # General dispatcher to the finer-grained deserialization methods.
    def Serialization.deserialize(type, str)
        case type
            when :gamestate then deserialize_gamestate(str)
            when :board then deserialize_board(str)
            when :tile then deserialize_tile(str)
            when :scores then deserialize_scores(str)
            when :history then deserialize_history(str)
        end
    end

    def Serialization.deserialize_gamestate(str)
        components = str.split("#", 4).map
        gamestate = Hash.new
        gamestate[:board] = deserialize_board(components[0])
        gamestate[:scores] =  deserialize_scores(components[1])
        gamestate[:turn] = components[2]
        gamestate[:history] = deserialize_history(components[3])
        gamestate
    end

    def Serialization.deserialize_scores(str)
        scorelst = [] 
        pairs = str.split("|").reject {|x| x.empty? }.map { |pair| pair.split("$", 2) }
        pairs.each do |arr|
            key = arr[0]
            val = arr[1].to_i
            scorelst << [key, val]
        end
        scorelst
    end

    def Serialization.deserialize_board(string)
        board = Board.new
        1.upto 15 do |row|
            1.upto 15 do |col|
                tile = string[0, 7]
                board[row][col] = Serialization.deserialize(:tile, tile)
                string = string.slice(8, string.length - 8)
            end
        end
        board
    end
    
    def Serialization.deserialize_tile(tile_string)
        tile = {}
    
        letter = tile_string[0,2]
        bonus = tile_string[2,1]
        location = tile_string[3,4]
    
        letter_type = case letter[0,1]
                          when "C" then :character
                          when "W" then :wildcard
                          when "-" then :none
                      end
        tile[:letter_type] = letter_type

        letter_value = case letter[1,1]
                           when "-" then :none
                           else letter[1,1]
                       end
        tile[:letter] = letter_value
    
        bonus_value = case bonus
                          when "D" then :double_word_score
                          when "T" then :triple_word_score
                          when "d" then :double_letter_score
                          when "t" then :triple_letter_score
                          when "n" then :none
                      end
        tile[:bonus] = bonus_value

        row_value = location[0,2].to_i
        col_value = location[2,2].to_i
    
        tile[:row] = row_value
        tile[:col] = col_value
        tile
    end
    
    def Serialization.deserialize_history(str)
        history = []
    
        until str.empty?
            triple = str.split("$", 3)
            name = triple[0]
            move = triple[1]
    
            pair = triple[2].split("|", 2)
            score = pair[0].to_i
    
            str = pair[1]
    
            move_as_list = move.split("|").reject { |x| x.empty? }.map { |tilestr| deserialize_tile(tilestr) }
            
            history << {:name => name, :move => move_as_list, :score => score}
        end
        history
    end




    def Serialization.serialize(what, obj)
        case what 
            when :gamestate then serialize_gamestate(obj)
            when :move then serialize_move(obj)
            when :tile then serialize_tile(obj)
            when :board then serialize_board(obj)
            when :scores then serialize_scores(obj)
            when :history then serialize_history(obj)
        end
    end


    def Serialization.serialize_gamestate(gamestate)
        board_str = serialize(:board, gamestate[:board])
        scores_str = serialize(:scores, gamestate[:scores])
        history_str = serialize(:history, gamestate[:history])

        board_str + "#" + scores_str + "#" + gamestate[:turn] + "#" + history_str
    end


    def Serialization.serialize_scores(lst)
        lst.map { |x| x[0] + "$" + x[1].to_s + "|" }.inject("") { |x,y| x + y }
    end


    def Serialization.serialize_board(board)
        lst = []
        1.upto(15) do |row|
            1.upto(15) do |col|
                lst << serialize(:tile, board.tiles[row][col])
            end
        end
        lst.map { |x| x + "|" }.inject("") { |x,y| x + y }
    end


    def Serialization.serialize_move(move)
        move.map { |m| serialize(:tile, m) + "|" }.inject("") { |x,y| x + y }
    end


    def Serialization.serialize_history(history)
        history.map do |hsh|
            player = hsh[:name]
            move = serialize(:move, hsh[:move])
            score = hsh[:score].to_s
            player + "$" + move + "$" + score + "|"
        end.inject("") { |x,y| x + y }
    end


    def Serialization.number_format(num)
        if num < 10 then "0" + num.to_s else num.to_s end
    end

    
    def Serialization.serialize_tile(tile)
        letter_type = case tile[:letter_type]
                          when :wildcard then "W"
                          when :character then "C"
                          when :none then "-"
                      end
        letter = case tile[:letter]
                     when :none then "-"
                     else tile[:letter]
                 end
        bonus = case tile[:bonus]
                    when :triple_letter_score then "t"
                    when :triple_word_score then "T"
                    when :double_letter_score then "d"
                    when :double_word_score then "D"
                    when :none then "n"
                end
        row = Serialization.number_format(tile[:row])
        col = Serialization.number_format(tile[:col])
        letter_type + letter + bonus + row + col
    end


end
