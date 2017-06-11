    class TypingTestModel {

/*
        // text to type
        var textLines of String
        // text typed in by the user
        var writtenLines of String
        var mistakes of Array of Boolean
        var isPaused;
        var isReady;
        var stayedInTheSameLine;

        var mistakesShadow of Array of Boolean
        var timeStarted;
        var timeFinished;
        var timesPaused of Date
*/

        constructor(text,
                plCharsOn, isReady = true) {
            this.MAX_LINE_LENGTH = 66;
            this.isReady = isReady;
            this.stayedInTheSameLine = false;
            if (!plCharsOn) {
                // TODO import Utils
                //text = Utils.shavePlChars(text);
                this.text = text;
            }
            //textLines = Utils.breakLines(text, MAX_LINE_LENGTH);
            this.textLines = [text];
            this.writtenLines = [""];
            this.mistakes = [[]];
            this.mistakesShadow = [[]];
            this.timesPaused = [];
        }

        onPrintableChar(c) {
            this.stayedInTheSameLine = true;
            if (c.length != 1) {
                throw new Error("parameter c must be a single character!");
            }
            if (this.timeStarted == null) {
                this.timeStarted = new Date();
            }
            if (this.timeFinished != null) {
                return false;
            }
            var last = this.writtenLines.length - 1;
            if (c == ' '
                    && this.writtenLines[last].length >= this.textLines[last].length) {
                return breakLine();
            }
            const isTypedCorrectly =
                this.writtenLines[last].length < this.textLines[last].length
                    && this.textLines[last].charAt(this.writtenLines[last].length) == c;
            this.writtenLines[last] += c;
            this.mistakes[last].push(!isTypedCorrectly);
            if (this.writtenLines[last].length > this.mistakesShadow[last].length) {
                this.mistakesShadow[last].push(!isTypedCorrectly);
            } else if (this.writtenLines[last].length > this.textLines[last].length
                    || this.textLines[last].charAt(
                        this.writtenLines[last].length - 1) != c) {
                this.mistakesShadow[last].splice(
                        this.writtenLines[last].length - 1, 0, !isTypedCorrectly);
            }
            if (this.writtenLines.length == this.textLines.length
                    && this.writtenLines[last].length >= this.textLines[last].length) {
                this.timeFinished = new Date();
            }
            return isTypedCorrectly;
        }

        onEnter() {
            stayedInTheSameLine = true;
            const last = writtenLines.length - 1;
            if (writtenLines[last].length >= textLines[last].length) {
                return breakLine();
            }
            return false;
        }

        onBackspace() {
            stayedInTheSameLine = true;
            if (timeStarted == null || timeFinished != null) {
                return;
            }
            const last = writtenLines.length - 1;
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

        get corrections() /* of Array of Boolean */ {
            var corrections /* of Array of Boolean */ = [];
            for (var i = 0; i < writtenLines.length; i++) {
                var line /* of Boolean */ = [];
                for (var j = 0; j < mistakes[i].length; j++) {
                    line.push(mistakesShadow[i][j] && !mistakes[i][j]);
                }
                corrections.push(line);
            }
            return corrections;
        }

        pause() {
            isPaused = true;
            timesPaused.push(new Date());
        }

        unpause() {
            isPaused = false;
            timesPaused.push(new Date());
        }

        get isMistakeMade() {
            for (var i = this.mistakes.length - 1; i >= 0; i--) {
                for (var j = this.mistakes[i].length - 1; j >= 0; j--) {
                    if (this.mistakes[i][j]) {
                        return true;
                    }
                }
            }
            return false;
        }

        get typingTimeInMilliseconds() {
            if (timeStarted == null) {
                return -1;
            } else {
                var timeElapsed = timeFinished;
                if (timeFinished == null) {
                    timeElapsed = new Date();
                }
                const interval = timeElapsed.time - timeStarted.time;
                // subtract paused time
                var pausedInterval = 0;
                for (var i = 0; i < timesPaused.length; i++) {
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

        get isStarted() {
            return this.timeStarted != null;
        }

        get isFinished() {
            return this.timeFinished != null;
        }

        breakLine() {
            if (timeStarted == null || timeFinished != null) {
                return false;
            }
            const last = writtenLines.length - 1;
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

