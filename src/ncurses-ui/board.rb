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

end
