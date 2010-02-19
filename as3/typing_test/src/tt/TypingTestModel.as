package tt {

    public class TypingTestModel {

        public static const MAX_LINE_LENGTH:int = 65;
        public static const LINE_BREAKERS:String = "\n ";

        // text to type
        public var textLines:Array /* of String */;
        // text typed in by the user
        public var writtenLines:Array /* of String */;

        public function TypingTestModel(text:String) {
            textLines = breakLines(text, MAX_LINE_LENGTH);
            writtenLines = [""];
        }

        public function onPrintableChar(c:String):void {
            if (c.length != 1) {
                throw new Error("parameter c must be a single character!");
            }
            writtenLines[writtenLines.length - 1] += c;
        }

        public function onEnter():Boolean {
            return tryBreakLine("\n");
        }

        private function tryBreakLine(c:String):Boolean {
            if (LINE_BREAKERS.indexOf(c) != -1) {
                writtenLines.push("");
                return true;
            }
            return false;
        }

        private static function breakLines(
                text:String, maxLineLength:int):Array /* of String */ {
            var multiSpace:RegExp = / +/g;
            var textLines:Array = text.replace(multiSpace, " ").split("\n");
            var lineEndIndex:int = 0;
            for (var i:int = 0; i < textLines.length; i++) {
                for (var j:int = 1; j < textLines[i].length
                        && j <= maxLineLength; j++) {
                    if (textLines[i].charAt(j) == " "
                            || textLines[i].charAt(j) == "\t") {
                        lineEndIndex = j;
                    }
                    if (j == maxLineLength) { // break line
                        textLines.splice(i + 1, 0,
                                textLines[i].substring(lineEndIndex + 1));
                        textLines[i] = textLines[i].substring(0, lineEndIndex);
                    }
                }
            }
            return textLines;
        }
    }
}
