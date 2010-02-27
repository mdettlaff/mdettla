package tt {

    import flash.events.Event;

    public class TypingTestEvent extends Event {

        public static const TYPING_TEST_ACTIVE:String = "typingTestActive";
        public static const NEW_TYPING_TEST:String = "newTypingTest";
        public static const TEST_RESULTS_UPDATE:String = "testResultsUpdate";
        public static const TYPING_STARTED:String = "typingStarted";
        public static const TYPING_TEST_FINISHED:String = "typingTestFinished";
        public static const PAUSE:String = "pause";
        public static const CONTINUE:String = "continue";
        public static const PL_CHARS_CHANGE:String = "plCharsChange";

        public var text:String;
        public var testResults:TestResults;

        public function TypingTestEvent(eventName:String,
                text:String = null, testResults:TestResults = null) {
            super(eventName, true);
            this.text = text;
            this.testResults = testResults;
        }
    }
}
