require 'board.rb'

# Class with static methods for serialization and deserialization of the 
# various datatypes that make up a gamestate.
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
        scorehash = {}
        pairs = str.split("|").reject {|x| x.empty? }.map { |pair| pair.split("$", 2) }
        pairs.each do |arr|
            key = arr[0]
            val = arr[1].to_i
            scorehash[key] = val
        end
        scorehash
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
end
