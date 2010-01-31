package
{
    import flash.display.LoaderInfo;
    import flash.display.Sprite;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.events.Event;

    public class Test extends Sprite
    {
        public function Test()
        {
            stage.addEventListener(Event.ENTER_FRAME, init);
        }

        public function init(event:Event):void
        {
            stage.removeEventListener(Event.ENTER_FRAME, init);

            var tf:TextField = new TextField();
            tf.autoSize = TextFieldAutoSize.LEFT;
            tf.border = true;
            addChild(tf);

            tf.appendText("params:" + "\n");
            try {
                var keyStr:String;
                var valueStr:String;
                var paramObj:Object =
                    LoaderInfo(this.root.loaderInfo).parameters;
                for (keyStr in paramObj) {
                    valueStr = String(paramObj[keyStr]);
                    tf.appendText("\t" + keyStr + ":\t" + valueStr + "\n");
                }
            } catch (error:Error) {
                tf.appendText(error.toString());
            }
        }
    }
}
