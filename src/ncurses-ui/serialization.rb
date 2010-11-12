
class Serialization

    def Serialization.deserialize_scores(str)
        str.split("|").reject {|x| x.empty? }.map { |pair| pair.split("$", 2) }
    end
    
    
    def Serialization.deserialize_tile(tile_string)
        tile = []
    
        letter = tile_string[0,2]
        bonus = tile_string[2,1]
        location = tile_string[3,4]
    
        letter_type = case letter[0,1]
                          when "C" then :character
                          when "W" then :wildcard
                          when "-" then :none
                      end
        letter_value = case letter[1,1]
                           when "-" then :none
                           else letter[1,1]
                       end
    
        letter_total = if letter_type == :none then :none else [letter_type, letter_value] end
    
        bonus_value = case bonus
                          when "D" then :double_word_score
                          when "T" then :triple_word_score
                          when "d" then :double_letter_score
                          when "t" then :triple_letter_score
                      end
        row_value = location[0,2].to_i
        col_value = location[2,2].to_i
    
        [letter_total, bonus_value, [row_value, col_value]]
    end
    
    
    
    def Serialization.deserialize_history(str)
        history = []
    
        until str.empty?
            pair = str.split("$", 2)
            name = pair[0]
    
            pair = pair[1].split("$", 2)
            move = pair[0]
    
            pair = pair[1].split("|", 2)
            score = pair[0]
    
            str = pair[1]
    
            move_as_list = move.split("|").reject { |x| x.empty? }.map { |tilestr| deserialize_tile(tilestr) }
            
            history << [name, move_as_list, score]
        end
    end

end
