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
            //this.MAX_LINE_LENGTH = 66;
            this.MAX_LINE_LENGTH = 40;
            this.isReady = isReady;
            this.stayedInTheSameLine = false;
            if (!plCharsOn) {
                text = new Utils().shavePlChars(text);
            }
            this.textLines = new Utils().breakLines(text, this.MAX_LINE_LENGTH);
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
                return this.breakLine();
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
            this.stayedInTheSameLine = true;
            const last = this.writtenLines.length - 1;
            if (this.writtenLines[last].length >= this.textLines[last].length) {
                return this.breakLine();
            }
            return false;
        }

        onBackspace() {
            this.stayedInTheSameLine = true;
            if (this.timeStarted == null || this.timeFinished != null) {
                return;
            }
            const last = this.writtenLines.length - 1;
            if (this.writtenLines[last].length > 0) {
                this.writtenLines[last] = this.writtenLines[last].substring(
                        0, this.writtenLines[last].length - 1);
                this.mistakes[last].pop();
            } else if (this.writtenLines.length > 1) {
                this.writtenLines.pop();
                this.mistakes.pop();
                this.stayedInTheSameLine = false;
            }
        }

        get corrections() /* of Array of Boolean */ {
            var corrections /* of Array of Boolean */ = [];
            for (var i = 0; i < this.writtenLines.length; i++) {
                var line /* of Boolean */ = [];
                for (var j = 0; j < this.mistakes[i].length; j++) {
                    line.push(this.mistakesShadow[i][j] && !this.mistakes[i][j]);
                }
                corrections.push(line);
            }
            return corrections;
        }

        pause() {
            this.isPaused = true;
            this.timesPaused.push(new Date());
        }

        unpause() {
            this.isPaused = false;
            this.timesPaused.push(new Date());
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
            if (this.timeStarted == null) {
                return -1;
            } else {
                var timeElapsed = this.timeFinished;
                if (this.timeFinished == null) {
                    timeElapsed = new Date();
                }
                const interval = timeElapsed.getTime() - this.timeStarted.getTime();
                // subtract paused time
                var pausedInterval = 0;
                for (var i = 0; i < this.timesPaused.length; i++) {
                    if (i % 2 == 0) {
                        pausedInterval -= this.timesPaused[i].getTime();
                    } else {
                        pausedInterval += this.timesPaused[i].getTime();
                    }
                }
                if (this.isPaused) {
                    pausedInterval += this.timesPaused[this.timesPaused.length - 1].getTime();
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
            if (this.timeStarted == null || this.timeFinished != null) {
                return false;
            }
            const last = this.writtenLines.length - 1;
            if (this.writtenLines.length < this.textLines.length) {
                this.writtenLines.push("");
                this.mistakes.push([]);
                if (this.mistakes.length > this.mistakesShadow.length) {
                    this.mistakesShadow.push([]);
                }
                this.stayedInTheSameLine = false;
            }
            return true;
        }
    }

