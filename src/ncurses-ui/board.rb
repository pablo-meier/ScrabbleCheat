require 'serialization'


# Datatype for the board.  Late-night Ruby means this is sloppy, the way
# we like it.  Refactors to come, I'm sure.

class Board

    alias_method :deserialize, :from_string
    attr_reader :tiles

    # As with the Erlang version, we're row-major, 1-index
    def initialize
        @tiles = []
        1.upto(15) { |i| @rows[i] = Array.new }
    end


    def deserialize(string)
        1.upto 15 do |row|
            1.upto 15 do |col|
                tile = string[0, 8]
                @tiles[row][col] = Serialization.deserialize_tile(tile)
                string = string.slice(8, string.length - 8)
            end
        end
    end

end
