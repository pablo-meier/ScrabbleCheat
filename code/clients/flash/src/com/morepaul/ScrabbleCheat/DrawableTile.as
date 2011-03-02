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
    import flash.display.Shape;
    
    /**
     * This Tile (inheriting from the Thrift-generated vanilla tile)
     * knows how to draw itself, and be something other than a Thrift
     * transportable container ^_^
     */
    public class DrawableTile extends Tile implements Drawable
    {

        private static const TRIPLE_WORD_COLOR:uint   = 0xDD3333;
        private static const DOUBLE_WORD_COLOR:uint   = 0xDD9999;
        private static const TRIPLE_LETTER_COLOR:uint = 0x3333DD;
        private static const DOUBLE_LETTER_COLOR:uint = 0x9999DD;
        private static const BASE_COLOR:uint          = 0xFFE600;

        private static const TILE_SIZE:int = 20;
        private static const TILE_BORDER_THICKNESS:int = 1;
        private static const TILE_BORDER_COLOR:uint = 0x000000;


        public function DrawableTile():void
        {
            super();
        }


        public function draw(canvas:Sprite):void
        {
            var rectangle:Shape = new Shape();

            var color:uint;
            switch (this.bonus)
            {
                case Bonus.TRIPLE_WORD_SCORE:
                    color = TRIPLE_WORD_COLOR;
                    break;
                case Bonus.DOUBLE_WORD_SCORE:
                    color = DOUBLE_WORD_COLOR;
                    break;
                case Bonus.TRIPLE_LETTER_SCORE:
                    color = TRIPLE_LETTER_COLOR;
                    break;
                case Bonus.DOUBLE_LETTER_SCORE:
                    color = DOUBLE_LETTER_COLOR;
                    break;
                case Bonus.NONE:
                    color = BASE_COLOR;
                    break;
            }

            var startx:int = (this.col - 1) * TILE_SIZE;
            var starty:int = (this.row - 1) * TILE_SIZE;

            // instantiate whatever you need to draw the glyph.
            rectangle.graphics.beginFill(color);
            rectangle.graphics.lineStyle(TILE_BORDER_THICKNESS, TILE_BORDER_COLOR);
            rectangle.graphics.drawRect(startx, starty, TILE_SIZE, TILE_SIZE);
            rectangle.graphics.endFill();

            canvas.addChild(rectangle);
        }
    }
}
