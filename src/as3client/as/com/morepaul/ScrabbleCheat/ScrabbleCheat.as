
package com.morepaul.ScrabbleCheat {

    import flash.display.Sprite;

    import flash.text.TextField;
    import flash.text.TextFormat;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFieldType;

    public class ScrabbleCheat extends Sprite {

        public function ScrabbleCheat():void
        {
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
