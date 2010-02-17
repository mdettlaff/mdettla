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
            typingArea = new TypingArea(
                    mainContainer.width, mainContainer.height);
            typingTestModel = new TypingTestModel(
                    "To jest sobie jakiś tekst, który chciałbym ładnie "
                    + "podzielić na wiele linii.\nTo jest druga linia.");
            typingArea.draw(typingTestModel);

            mainContainer.addChild(typingArea);

            keyEventDispatcher.addEventListener(
                    KeyboardEvent.KEY_DOWN, onKeyDown);
        }

        private function onKeyDown(event:KeyboardEvent):void {
            if (event.charCode >= 32) { // not a control character
                typingTestModel.onRegularChar(plCharFrom(event));
                typingArea.draw(typingTestModel);
            }
            if (event.keyCode == Keyboard.ENTER) {
                typingTestModel.onEnter();
                typingArea.draw(typingTestModel);
            }
        }

        private static function plCharFrom(event:KeyboardEvent):String {
            const EN:String = "aAcCeElLnNoOsSzZxX";
            const PL:String = "ąĄćĆęĘłŁńŃóÓśŚżŻźŹ";
            var index:int;
            if (event.altKey) {
                index = EN.indexOf(String.fromCharCode(event.charCode));
                if (index != -1) {
                    return String.fromCharCode(PL.charCodeAt(index));
                }
            }
            return String.fromCharCode(event.charCode);
        }
    }
}
