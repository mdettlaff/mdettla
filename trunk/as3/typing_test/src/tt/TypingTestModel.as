package tt {

    import mx.utils.StringUtil;

    public class TypingTestModel {

        public static const MAX_LINE_LENGTH:int = 70;
        public static const LINE_BREAKERS:String = "\n ";

        // text to type
        public var textLines:Array /* of String */;
        // text typed in by the user
        public var writtenLines:Array /* of String */;
        public var mistakes:Array /* of String */;
        public var mistakesShadow:Array /* of String */;

        private var timeStarted:Date;
        private var timeFinished:Date;

        public function TypingTestModel(text:String) {
            textLines = breakLines(text, MAX_LINE_LENGTH);
            writtenLines = [""];
            mistakes = [""];
            mistakesShadow = [""];
        }

        public function onPrintableChar(c:String):Boolean {
            if (c.length != 1) {
                throw new Error("parameter c must be a single character!");
            }
            var isTypedCorrectly:Boolean = true;
            if (timeStarted == null) {
                timeStarted = new Date();
            }
            if (timeFinished != null) {
                return false;
            }
            var last:int = writtenLines.length - 1;
            if (c == ' ' && LINE_BREAKERS.indexOf(c) != -1
                    && writtenLines[last].length >= textLines[last].length) {
                return tryBreakLine(' ');
            }
            var correctChar:String;
            var incorrectChar:String;
            if (writtenLines[last].length < textLines[last].length &&
                    textLines[last].charAt(writtenLines[last].length) == c) {
                correctChar = c;
                incorrectChar = ' ';
            } else {
                correctChar = ' ';
                if (c != ' ') {
                    incorrectChar = c;
                } else {
                    incorrectChar = '_';
                }
                isTypedCorrectly = false;
            }
            writtenLines[last] += correctChar;
            mistakes[last] += incorrectChar;
            if (writtenLines[last].length > mistakesShadow[last].length) {
                mistakesShadow[last] += incorrectChar;
            } else if (writtenLines[last].length > textLines[last].length
                    || textLines[last].charAt(
                        writtenLines[last].length - 1) != c) {
                var cIndex:int = writtenLines[last].length - 1;
                mistakesShadow[last] = mistakesShadow[last].slice(0, cIndex)
                    + c + mistakesShadow[last].slice(cIndex + 1);
            }
            return isTypedCorrectly;
        }

        public function onEnter():Boolean {
            return tryBreakLine('\n');
        }

        public function onBackspace():void {
            if (timeStarted == null || timeFinished != null) {
                return;
            }
            var lastLine:String = writtenLines[writtenLines.length - 1];
            if (lastLine.length > 0) {
                writtenLines[writtenLines.length - 1] =
                    lastLine.substring(0, lastLine.length - 1);
                mistakes[mistakes.length - 1] =
                    mistakes[mistakes.length - 1].substring(
                            0, lastLine.length - 1);
            } else if (writtenLines.length > 1) {
                writtenLines.pop();
                mistakes.pop();
            }
        }

        private function tryBreakLine(c:String):Boolean {
            if (timeStarted == null || timeFinished != null) {
                return false;
            }
            if (LINE_BREAKERS.indexOf(c) == -1) {
                return false;
            }
            var last:int = writtenLines.length - 1;
            if (writtenLines[last].length >= textLines[last].length) {
                if (writtenLines.length < textLines.length) {
                    writtenLines.push("");
                    mistakes.push("");
                    if (mistakes.length > mistakesShadow.length) {
                        mistakesShadow.push("");
                    }
                } else {
                    timeFinished = new Date();
                }
                return true;
            }
            return false;
        }

        private static function breakLines(
                text:String, maxLineLength:int):Array /* of String */ {
            var multiSpace:RegExp = / +/g;
            var textLines:Array =
                StringUtil.trim(text.replace(multiSpace, ' ')).split('\n');
            var lineEndIndex:int = 0;
            for (var i:int = 0; i < textLines.length; i++) {
                for (var j:int = 1; j < textLines[i].length
                        && j <= maxLineLength; j++) {
                    if (textLines[i].charAt(j) == ' '
                            || textLines[i].charAt(j) == '\t') {
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
