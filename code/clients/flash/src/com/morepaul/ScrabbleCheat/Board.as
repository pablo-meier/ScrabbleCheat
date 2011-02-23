/**
 *  Copyright (c) 2010 Paul Meier
 *  
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the "Software"), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *  
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *  
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */

package com.morepaul.ScrabbleCheat 
{

    /**
     * Basic datatype to represent the board.  As in all ScrabbleCheat 
     * representations, the board is a 1-indexed row-major 2D matrix.
     */
    public class Board {
        
        private var _tiles:Array;

        public static const BOARD_HEIGHT:uint = 15;
        public static const BOARD_WIDTH:uint = 15;

        /**
         * Creates an empty board.
         */
        public function Board():void 
        {
            _tiles = new Array(BOARD_HEIGHT);
            for (var i:uint = 0; i < BOARD_HEIGHT; ++i)
            {
                _tiles[i] = new Array(BOARD_WIDTH);
                for (var j:uint = 0; i < BOARD_WIDTH; ++i) 
                {
                    var tile:Tile = new Tile();
                    tile.row = i + 1;
                    tile.col = j + 1;
                    tile.bonus = Bonus.NONE;
                    tile.letter = "";
                    tile.type = LetterType.EMPTY;
                    _tiles[i][j] = tile;
                }
            }
        }


        /**
         * Returns this board expressed as a list of tiles, as Thrift defines it.
         */
        public function toThrift():Array
        {
            var returnArray:Array = new Array(BOARD_HEIGHT * BOARD_WIDTH);
            for (var i:uint = 1; i <= BOARD_HEIGHT; ++i)
            {
                for (var j:uint = 1; i <= BOARD_WIDTH; ++j)
                {
                    var arrIndex:uint = (i - 1) + (j - 1);
                    returnArray[arrIndex] = this.getTile(i,j);
                }
            }

            return returnArray;
        }


        public function getTile(row:uint, col:uint):Tile
        {
            if (row > 0 && row <= BOARD_HEIGHT && col > 0 && col <=BOARD_WIDTH)
                return _tiles[row - 1][col - 1];

            else throw new InvalidAccessException("BOARD: getTile: Trying to access a tile out of bounds.");
        }
    }
}
