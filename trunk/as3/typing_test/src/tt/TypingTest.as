package tt {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.events.IEventDispatcher;
    import flash.events.KeyboardEvent;
    import flash.ui.Keyboard;

    public class TypingTest {

        private var typingArea:TypingArea;
        private var typingTestModel:TypingTestModel;

        public function TypingTest(mainContainer:DisplayObjectContainer,
                keyEventDispatcher:IEventDispatcher = null) {
            if (keyEventDispatcher == null) {
                keyEventDispatcher = mainContainer;
            }
            this.typingArea =
                new TypingArea(mainContainer.width, mainContainer.height);
            mainContainer.addChild(typingArea);

            typingTestModel = new TypingTestModel();

            keyEventDispatcher.addEventListener(
                    KeyboardEvent.KEY_DOWN, onKeyDown);
        }

        private function onKeyDown(event:KeyboardEvent):void {
            if (event.charCode >= 32) { // not a control character
                typingTestModel.onRegularChar(plCharFrom(event));
                typingArea.draw(typingTestModel);
            }
        }

        private static function plCharFrom(event:KeyboardEvent):String {
            const EN:String = "acelnoszx";
            const PL:String = "ąćęłńóśżź";
            var index:int;
            if (event.altKey) {
                index = EN.indexOf(String.fromCharCode(event.charCode));
                if (index != -1) {
                    return String.fromCharCode(PL.charCodeAt(index));
                }
                index = EN.toUpperCase().indexOf(
                        String.fromCharCode(event.charCode));
                if (index != -1) {
                    return String.fromCharCode(
                            PL.toUpperCase().charCodeAt(index));
                }
            }
            return String.fromCharCode(event.charCode);
        }
    }
}
