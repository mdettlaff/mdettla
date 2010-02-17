package tt {

    public class TypingTestModel {

        private var typingArea:TypingArea;

        public var textLines:Array /* of String */;

        public function TypingTestModel() {
            textLines = [];
            textLines.push("początek linii");
        }

        public function onRegularChar(c:String):void {
            if (c.length != 1) {
                throw new Error("parameter c must be a single character!");
            }
            textLines[0] += c;
        }
    }
}
