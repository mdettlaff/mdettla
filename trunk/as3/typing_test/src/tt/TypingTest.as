package tt {

    import flash.display.DisplayObjectContainer;
    import flash.display.Sprite;
    import flash.events.IEventDispatcher;
    import flash.events.Event;
    import flash.events.KeyboardEvent;
    import flash.events.TimerEvent;
    import flash.ui.Keyboard;
    import flash.utils.setTimeout;
    import flash.utils.Timer;

    public class TypingTest {

        private var typingArea:TypingArea;
        private var typingTestModel:TypingTestModel;
        private var polishChars:PolishChars;
        private var updateTimer:Timer;

        public function TypingTest(mainContainer:DisplayObjectContainer,
                mainEventDispatcher:IEventDispatcher) {
            polishChars = new PolishChars();
            typingArea = new TypingArea(
                    mainContainer.width, mainContainer.height);
            typingTestModel = new TypingTestModel(
                    "To jest tekst.\nNa temat żółwia.\n"); // DEBUG
            typingArea.draw(typingTestModel);

            mainContainer.addChild(typingArea);

            mainEventDispatcher.addEventListener(
                    Event.ACTIVATE, onActivate);
            mainEventDispatcher.addEventListener(
                    KeyboardEvent.KEY_DOWN, onKeyDown);
            mainContainer.addEventListener(
                    TypingTestEvent.NEW_TYPING_TEST, onNewTypingTest);
            mainContainer.addEventListener(
                    TypingTestEvent.PAUSE, onPause);
            mainContainer.addEventListener(
                    TypingTestEvent.CONTINUE, onContinue);

            updateTimer = new Timer(1000, 0);
            updateTimer.addEventListener(TimerEvent.TIMER, onUpdateTimer);
            updateTimer.start();
        }

        private function onKeyDown(event:KeyboardEvent):void {
            var wasStarted:Boolean = typingTestModel.isStarted;
            if (!typingTestModel.isFinished && !typingTestModel.isPaused) {
                if (event.charCode >= 32) { // not a control character
                    var c:String = polishChars.charFrom(event);
                    if (c != null) {
                        typingTestModel.onPrintableChar(c);
                    }
                } else if (event.keyCode == Keyboard.ENTER) {
                    typingTestModel.onEnter();
                } else if (event.keyCode == Keyboard.BACKSPACE) {
                    typingTestModel.onBackspace();
                }
                if (typingTestModel.isFinished) {
                    typingArea.dispatchEvent(
                            new TypingTestEvent(
                                TypingTestEvent.TYPING_TEST_FINISHED,
                                null, new TestResults(typingTestModel)));
                }
                if (!wasStarted && typingTestModel.isStarted) {
                    typingArea.dispatchEvent(new TypingTestEvent(
                                TypingTestEvent.TYPING_STARTED));
                }
                typingArea.draw(typingTestModel);
            }
        }

        private function onActivate(event:Event):void {
            typingArea.removeWelcomeText();
            setTimeout(dispatchTypingTestActive, 500);
        }

        private function onNewTypingTest(event:TypingTestEvent):void {
            typingTestModel = new TypingTestModel(event.text);
            typingArea.draw(typingTestModel);
        }

        private function onUpdateTimer(event:TimerEvent):void {
            typingArea.dispatchEvent(
                    new TypingTestEvent(TypingTestEvent.TEST_RESULTS_UPDATE,
                        null, new TestResults(typingTestModel)));
        }

        private function onPause(event:TypingTestEvent):void {
            updateTimer.stop();
            typingTestModel.pause();
        }

        private function onContinue(event:TypingTestEvent):void {
            typingTestModel.unpause();
            updateTimer.start();
        }

        private function dispatchTypingTestActive():void {
            typingArea.dispatchEvent(
                    new TypingTestEvent(TypingTestEvent.TYPING_TEST_ACTIVE));
        }
    }
}
