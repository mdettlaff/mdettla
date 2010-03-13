package tt {

    public class TypingTestModel {

        public static const MAX_LINE_LENGTH:int = 68;

        // text to type
        public var textLines:Array /* of String */;
        // text typed in by the user
        public var writtenLines:Array /* of String */;
        public var mistakes:Array /* of Array of Boolean */;
        public var isPaused:Boolean;
        public var isReady:Boolean;
        public var stayedInTheSameLine:Boolean;

        private var mistakesShadow:Array /* of Array of Boolean */;
        private var timeStarted:Date;
        private var timeFinished:Date;
        private var timesPaused:Array /* of Date */;

        public function TypingTestModel(text:String,
                plCharsOn:Boolean, isReady:Boolean = true) {
            this.isReady = isReady;
            this.stayedInTheSameLine = false;
            if (!plCharsOn) {
                text = Utils.shavePlChars(text);
            }
            textLines = Utils.breakLines(text, MAX_LINE_LENGTH);
            writtenLines = [""];
            mistakes = [[]];
            mistakesShadow = [[]];
            timesPaused = [];
        }

        public function onPrintableChar(c:String):Boolean {
            stayedInTheSameLine = true;
            if (c.length != 1) {
                throw new Error("parameter c must be a single character!");
            }
            if (timeStarted == null) {
                timeStarted = new Date();
            }
            if (timeFinished != null) {
                return false;
            }
            var last:int = writtenLines.length - 1;
            if (c == ' '
                    && writtenLines[last].length >= textLines[last].length) {
                return breakLine();
            }
            const isTypedCorrectly:Boolean =
                writtenLines[last].length < textLines[last].length
                    && textLines[last].charAt(writtenLines[last].length) == c;
            writtenLines[last] += c;
            mistakes[last].push(!isTypedCorrectly);
            if (writtenLines[last].length > mistakesShadow[last].length) {
                mistakesShadow[last].push(!isTypedCorrectly);
            } else if (writtenLines[last].length > textLines[last].length
                    || textLines[last].charAt(
                        writtenLines[last].length - 1) != c) {
                mistakesShadow[last].splice(
                        writtenLines[last].length - 1, 0, !isTypedCorrectly);
            }
            if (writtenLines.length == textLines.length
                    && writtenLines[last].length >= textLines[last].length) {
                timeFinished = new Date();
            }
            return isTypedCorrectly;
        }

        public function onEnter():Boolean {
            stayedInTheSameLine = true;
            const last:int = writtenLines.length - 1;
            if (writtenLines[last].length >= textLines[last].length) {
                return breakLine();
            }
            return false;
        }

        public function onBackspace():void {
            stayedInTheSameLine = true;
            if (timeStarted == null || timeFinished != null) {
                return;
            }
            const last:int = writtenLines.length - 1;
            if (writtenLines[last].length > 0) {
                writtenLines[last] = writtenLines[last].substring(
                        0, writtenLines[last].length - 1);
                mistakes[last].pop();
            } else if (writtenLines.length > 1) {
                writtenLines.pop();
                mistakes.pop();
                stayedInTheSameLine = false;
            }
        }

        public function get corrections():Array /* of Array of Boolean */ {
            var corrections:Array /* of Array of Boolean */ = [];
            for (var i:int = 0; i < writtenLines.length; i++) {
                var line:Array /* of Boolean */ = [];
                for (var j:int = 0; j < mistakes[i].length; j++) {
                    line.push(mistakesShadow[i][j] && !mistakes[i][j]);
                }
                corrections.push(line);
            }
            return corrections;
        }

        public function pause():void {
            isPaused = true;
            timesPaused.push(new Date());
        }

        public function unpause():void {
            isPaused = false;
            timesPaused.push(new Date());
        }

        public function get isMistakeMade():Boolean {
            for (var i:int = mistakes.length - 1; i >= 0; i--) {
                for (var j:int = mistakes[i].length - 1; j >= 0; j--) {
                    if (mistakes[i][j]) {
                        return true;
                    }
                }
            }
            return false;
        }

        public function get typingTimeInMilliseconds():Number {
            if (timeStarted == null) {
                return -1;
            } else {
                var timeElapsed:Date = timeFinished;
                if (timeFinished == null) {
                    timeElapsed = new Date();
                }
                const interval:Number = timeElapsed.time - timeStarted.time;
                // subtract paused time
                var pausedInterval:Number = 0;
                for (var i:Number = 0; i < timesPaused.length; i++) {
                    if (i % 2 == 0) {
                        pausedInterval -= timesPaused[i].time;
                    } else {
                        pausedInterval += timesPaused[i].time;
                    }
                }
                if (isPaused) {
                    pausedInterval += timesPaused[timesPaused.length - 1].time;
                }
                return interval - pausedInterval;
            }
        }

        public function get isStarted():Boolean {
            return timeStarted != null;
        }

        public function get isFinished():Boolean {
            return timeFinished != null;
        }

        private function breakLine():Boolean {
            if (timeStarted == null || timeFinished != null) {
                return false;
            }
            const last:int = writtenLines.length - 1;
            if (writtenLines.length < textLines.length) {
                writtenLines.push("");
                mistakes.push([]);
                if (mistakes.length > mistakesShadow.length) {
                    mistakesShadow.push([]);
                }
                stayedInTheSameLine = false;
            }
            return true;
        }
    }
}
