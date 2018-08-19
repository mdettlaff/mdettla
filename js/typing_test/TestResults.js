class TestResults {

    constructor(typingTestModel, timeSecondsVerifier = 0) {
        this.timeSecondsVerifier = timeSecondsVerifier;
        this.writtenCharsCount = 0;
        for (var writtenLine of typingTestModel.writtenLines) {
            this.writtenCharsCount += writtenLine.length;
        }
        this.writtenCharsCount += typingTestModel.writtenLines.length - 1;
        this.mistakesCount = this.countMistakes(typingTestModel);
        this.correctionsCount = 0;
        for (var i = 0; i < typingTestModel.corrections.length; i++) {
            var correctionsLine = typingTestModel.corrections[i];
            for (var isCorrection of correctionsLine) {
                this.correctionsCount += isCorrection ? 1 : 0;
            }
        }
        this.typingTimeInMilliseconds = typingTestModel.typingTimeInMilliseconds;
        // TODO replace "some" function
        //this.plChars = typingTestModel.textLines.some(Utils.containsPlChars);
    }

    get speed() {
        if (this.writtenCharsCount == 0) {
            return 0;
        }
        return this.writtenCharsCount / this.timeMinutes;
    }

    get realSpeed() {
        if (this.writtenCharsCount == 0) {
            return 0;
        }
        return (this.writtenCharsCount - this.mistakesCount) / this.timeMinutes;
    }

    get realSpeedWPM() {
        return this.realSpeed / 5;
    }

    get correctness() {
        if (this.writtenCharsCount == 0) {
            return 0;
        }
        return (this.writtenCharsCount - this.mistakesCount - this.correctionsCount)
            / this.writtenCharsCount * 100;
    }

    get timeMinutes() {
        return this.timeSeconds / 60;
    }

    get timeSeconds() {
        return this.typingTimeInMilliseconds / 1000;
    }

    toHTMLString() {
        var html = "<font size=\"14\">prędkość: <b>"
            + this.realSpeed.toFixed(1) + "</b> znaków/min "
            + "(" + this.realSpeedWPM.toFixed(1) + " słów/min)\n"
            + "poprawność: <b>" + this.correctness.toFixed(1) + "</b>%"
            + "</font><font size = \"12\">\n\n"
            + "Ilość błędów: " + (this.mistakesCount + this.correctionsCount);
        if (this.correctionsCount > 0 && this.mistakesCount == 0) {
            html += ", wszystkie poprawione.\n";
        } else if (this.mistakesCount > 0) {
            html += ", z czego poprawiono " + this.correctionsCount + ".\n";
        } else {
            html += ".\n";
        }
        html += "Przepisano "
            + (this.writtenCharsCount - this.mistakesCount) + " znaków w czasie ";
        if (int(this.timeMinutes) > 0) {
            html += int(this.timeMinutes) + " min ";
        }
        html += (int(this.timeSeconds) % 60) + " s." + "</font>";
        return html;
    }

    countMistakes(typingTestModel) {
        var mistakesCount = 0;
        for (var i = 0; i < typingTestModel.mistakes.length; i++) {
            var mistakesLine = typingTestModel.mistakes[i];
            for (var isMistake of mistakesLine) {
                mistakesCount += isMistake ? 1 : 0;
            }
            if (mistakesLine.length
                    != typingTestModel.textLines[i].length) {
                if (i < typingTestModel.mistakes.length - 1
                        || typingTestModel.isFinished) {
                    mistakesCount += 1; // +1 for incorrectly typed Enter
                }
            }
        }
        return mistakesCount;
    }
}
