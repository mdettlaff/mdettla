package tt {

    import flash.events.Event;

    public class TypingTestEvent extends Event {

        public static const NEW_TYPING_TEST:String = "newTypingTest";
        public static const PAUSE:String = "pause";
        public static const CONTINUE:String = "continue";

        public var text:String;

        public function TypingTestEvent(eventName:String, text:String = null) {
            super(eventName, true);
            this.text = text;
        }
    }
}
