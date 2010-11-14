require 'serialization'


# Datatype for the board.  Late-night Ruby means this is sloppy, the way
# we like it.  Refactors to come, I'm sure.

class Board

    attr_accessor :tiles

    # As with the Erlang version, we're row-major, 1-index
    def initialize
        @tiles = []
        1.upto(15) { |i| @rows[i] = Array.new }
    end


end
