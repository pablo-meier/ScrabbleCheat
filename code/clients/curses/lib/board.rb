# Datatype for the board. Late-night Ruby means this is sloppy, the way
# we like it. Refactors to come, I'm sure.
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


    # Places a move on this board.  Note that move is the move hash,
    # including score.
    def place_move(movehash)
        movetiles = movehash[:move]
        movetiles.each do |tile|
            row, col = tile[:row], tile[:col] 
            @tiles[row][col] = tile
        end
        :done
    end

    # Returns a Thrift-friendly version of itself.  Since Thrift defines a board
    # as simply a list of tiles, we'll just make a flat array of ourselves.
    def to_list
        retval = []
        1.upto(@tiles.length - 1) do |row|
            1.upto(@tiles[row].length - 1) do |col|
                retval << @tiles[row][col]
            end
        end
        retval
    end


    # Given a list of native tiles, makes a board!
    def Board.from_list(thrift_board)
        board = Board.new
        0.upto(14) do |row|
            0.upto(14) do |col|
                board.tiles[row + 1][col + 1] = thrift_board[(row * 15) + col]
            end
        end
        board
    end


    # Object.dup or Object.clone provide shallow copies, which is 
    # great unless you want to modify @tiles... which is almost certainly
    # what you want to do.  This provides a real copy of the board.  
    def deep_copy
        Marshal.load(Marshal.dump(self))
    end
end
