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

    public class ScrabbleCheatClientMain extends Sprite 
    {

        private static const BG_COLOR:uint = 0x666666;

        private var m_board:Board;
        private var m_connection:ScrabbleCheat;


        private var m_debug:TextField;

        public function ScrabbleCheatClientMain():void
        {
            m_connection = new ThriftLayer();

            var playerList:Array = ["Paul", "Sam"];
            m_connection.new_game(playerList, onNewGameFailure, onNewGameSuccess);


            m_debug = new TextField();
            m_debug.x = (stage.stageWidth / 2) - (m_debug.width / 2);
            m_debug.y = (stage.stageHeight / 2) - (m_debug.height / 2) + 200;
            m_debug.text = "Started up.";
            this.addChild(m_debug);
        }


        private function onNewGameSuccess(fresh:Gamestate):void
        {
            m_board = Board.fromThrift(fresh.board);

            m_board.draw(this);
            m_debug.text = "Connection Successful!";
        }


        private function onNewGameFailure(err:String):void
        {
            m_debug.text = "Connection Failed! Msg: " + err;
        }
    }
}
