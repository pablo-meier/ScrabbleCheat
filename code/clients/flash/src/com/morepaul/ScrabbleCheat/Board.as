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

    import flash.display.Sprite;

    /**
     * Basic datatype to represent the board.  As in all ScrabbleCheat 
     * representations, the board is a 1-indexed row-major 2D matrix.
     */
    public class Board implements Drawable
    {
        
        private var m_tiles:Array;
        private var m_frozen:Boolean;

        public static const BOARD_HEIGHT:int = 15;
        public static const BOARD_WIDTH:int = 15;


        /**
         * Creates an empty board.
         */
        public function Board():void 
        {
            m_frozen = false;

            m_tiles = new Array(BOARD_HEIGHT);
            for (var i:int = 0; i < m_tiles.length; ++i)
            {
                m_tiles[i] = new Array(BOARD_WIDTH);
                for (var j:int = 0; j < m_tiles[i].length; ++j) 
                {
                    var tile:Tile = new DrawableTile();
                    tile.row = i + 1;
                    tile.col = j + 1;
                    tile.bonus = Bonus.NONE;
                    tile.letter = "";
                    tile.type = LetterType.EMPTY;

                    m_tiles[i][j] = tile;
                }
            }
        }


        /**
         * Returns this board expressed as a list of tiles, as Thrift defines it.
         */
        public function toThrift():Array
        {
            var returnArray:Array = new Array(BOARD_HEIGHT * BOARD_WIDTH);
            for (var i:int = 1; i <= BOARD_HEIGHT; ++i)
            {
                for (var j:int = 1; j <= BOARD_WIDTH; ++j)
                {
                    var arrIndex:uint = (i - 1) + (j - 1);
                    returnArray[arrIndex] = this.getTile(i,j) as Tile;
                }
            }

            return returnArray;
        }


        /**
         * Constructs a board from the bare Thrift array of plain tiles.
         */
        public static function fromThrift(arr:Array):Board
        {
            var retBoard:Board = new Board();
            for (var i:int = 0; i < BOARD_HEIGHT; ++i)
            {
                for (var j:int = 0; j < BOARD_WIDTH; ++j)
                {
                    var index:int = (i * BOARD_HEIGHT) + j;
                    var dTile:DrawableTile = arr[index] as DrawableTile;

                    retBoard.setTile( i + 1, j + 1, dTile);
                }
            }
            retBoard.freeze();

            return retBoard;
        }


        public function getTile(row:int, col:int):DrawableTile
        {
            if (row > 0 && row <= BOARD_HEIGHT && col > 0 && col <= BOARD_WIDTH)
                return m_tiles[row - 1][col - 1];

            else throw new InvalidAccessException("BOARD: getTile: Trying to access a tile out of bounds: row = " +
                                                   row + ", col = " + col);
        }


        public function setTile(row:int, col:int, tile:Tile):void
        {
            if ( this.isFrozen() )
            {
                m_tiles[row - 1][col - 1] = tile;
            }
        }


        private function isFrozen():Boolean
        {
            return m_frozen;
        }

        private function freeze():void
        {
            m_frozen = true;
        }


        public function draw(canvas:Sprite):void
        {
            for (var i:int = 1; i <= BOARD_HEIGHT; ++i)
            {
                var row:Array = m_tiles[i];
                for (var j:int = 1; j <= BOARD_WIDTH; ++j)
                {
                    var tile:DrawableTile = this.getTile(i, j);
                    tile.draw(canvas);
                }
            }
        }
    }
}
