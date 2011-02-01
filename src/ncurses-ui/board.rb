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
    def to_thrift
        retval = []
        @tiles.each do |row|
            row.each do |tile|
                retval << tile
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
