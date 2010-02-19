package tt {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.events.IEventDispatcher;
    import flash.events.KeyboardEvent;
    import flash.ui.Keyboard;

    public class TypingTest {

        private var typingArea:TypingArea;
        private var typingTestModel:TypingTestModel;
        private var polishChars:PolishChars;

        public function TypingTest(mainContainer:DisplayObjectContainer,
                keyEventDispatcher:IEventDispatcher = null) {
            if (keyEventDispatcher == null) {
                keyEventDispatcher = mainContainer;
            }
            polishChars = new PolishChars();
            typingArea = new TypingArea(
                    mainContainer.width, mainContainer.height);
            typingTestModel = new TypingTestModel(
                    "To jest sobie jakiś tekst, który chciałbym ładnie "
                    + "podzielić na wiele linii.\nTo jest druga linia.");
            typingArea.draw(typingTestModel);

            mainContainer.addChild(typingArea);

            keyEventDispatcher.addEventListener(
                    KeyboardEvent.KEY_DOWN, onKeyDown);
            mainContainer.addEventListener(
                    TypingTestEvent.NEW_TYPING_TEST, onNewTypingTest);
        }

        private function onKeyDown(event:KeyboardEvent):void {
            if (event.charCode >= 32) { // not a control character
                var c:String = polishChars.charFrom(event);
                if (c != null) {
                    typingTestModel.onPrintableChar(c);
                }
                typingArea.draw(typingTestModel);
            } else if (event.keyCode == Keyboard.ENTER) {
                typingTestModel.onEnter();
                typingArea.draw(typingTestModel);
            }
        }

        private function onNewTypingTest(event:TypingTestEvent):void {
            typingTestModel = new TypingTestModel(event.text);
            typingArea.draw(typingTestModel);
        }
    }
}
