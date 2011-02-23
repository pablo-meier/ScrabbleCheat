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


/**
 * Top-level package, and main window the of the application.
 */
package com.morepaul.ScrabbleCheat 
{

    import flash.display.Sprite;

    import flash.text.TextField;
    import flash.text.TextFormat;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFieldType;

    public class ScrabbleCheatClientMain extends Sprite 
    {

        private var _board:Board;

        public function ScrabbleCheat():void
        {
            _board = new Board();
            _board.toThrift();
            
            var tf:TextField = new TextField();
            tf.text = "HELLO PAUL THIS WILL BE BRILLIANT!";

            var format:TextFormat = new TextFormat();
            format.font = "_sans";
            tf.defaultTextFormat = format;

            tf.x = (stage.stageWidth / 2) - (tf.width / 2);
            tf.y = (stage.stageHeight / 2) - (tf.height/ 2);
            this.addChild(tf);
        }
    }
}
