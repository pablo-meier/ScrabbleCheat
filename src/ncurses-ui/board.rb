require 'serialization'


# Datatype for the board.  Late-night Ruby means this is sloppy, the way
# we like it.  Refactors to come, I'm sure.

class Board

    attr_accessor :tiles

    # As with the Erlang version, we're row-major, 1-index
    def initialize
        @tiles = []
        1.upto(15) { |i| @tiles[i] = Array.new }
    end

    def [](val)
        @tiles[val]
    end

    def[]=(val, asgn)
        @tiles[val] = asgn
    end

    def eql?(other)
        if other.instance_of? Board
            other.tiles == self.tiles
        else
            false
        end
    end

end
