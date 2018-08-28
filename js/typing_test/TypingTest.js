class TypingTest {

constructor() {
  this.mockTexts = ['W zeszły czwartek dwa rekiny ludojady pożarły osiemnastoletniego australijskiego surfera. Według świadków zdarzenia, rozerwały jego ciało na pół, a następnie spędziły parę minut walcząc o to, któremu z nich przypadnie który kawałek. Jak zwykle w takim przypadku, przeprowadzono wywiady z różnymi ekspertami od przyrody, którzy zgodnie stwierdzili, że rekiny te należy wypuścić na wolność po udzieleniu im pouczenia, częściowo dlatego, że są pod ochroną, a częściowo dlatego, że do takich ataków dochodzi niezwykle rzadko.', 'Yes this is także pies, może jeszcze z jedną linijką.', 'jeszcze jeden'];
  this.mockTextIndex = -1;

  this.splashScreenVisible = true;
  this.plCharsOn = true;
  this.hData = '';

  this.textWithPlChars = null;
  this.canvas = document.getElementById("typing_area");
  this.context = this.canvas.getContext("2d");
}

init() {
  this.textWithPlChars = this.nextText();

  this.model = new TypingTestModel(this.textWithPlChars, this.plCharsOn);
  this.typingArea = new TypingArea(this.context, this.canvas.width, this.canvas.height);

  this.draw();
  this.updateInProgressResults();
  this.addEventListeners();

  this.preventBackspaceNavigation();

  this.hideSplashScreen(); // hide splash screen for now to make testing easier
}

addEventListeners() {
  this.canvas.addEventListener('keydown', this.handleKeyPress.bind(this));
  document.getElementById('plCharsCheckbox').addEventListener('change', this.handlePlCharsCheckboxChange.bind(this));
  document.getElementById('newTestButton').addEventListener('click', this.handleNewTestButtonClick.bind(this));
  document.getElementById('splash_screen').addEventListener('click', this.hideSplashScreen.bind(this));
  document.getElementById('ok_button').addEventListener('click', this.hideDialog.bind(this));
}

draw() {
  this.typingArea.draw(this.model);
}

handleKeyPress(e) {
  if (this.model.isReady && !this.model.isFinished && !this.model.isPaused) {
    if (e.keyCode == 8 /* backspace */) {
      this.model.onBackspace();
      this.draw();
    } else if (e.keyCode == 13 /* enter */) {
      this.model.onEnter();
      this.draw();
    } else if (e.keyCode >= 32 /* not a control character */
        && e.key != null && e.key.length == 1) {
      this.model.onPrintableChar(e.key);
      this.draw();
    }
    if (this.model.isFinished) {
      this.showDialog();
      this.submitTestResults();
    }
  }
}

updateInProgressResults() {
  var results = new TestResults(this.model);
  var inProgressResultsSpeed = 'prędkość: ' + results.realSpeed.toFixed(1) + ' znaków/min';
  var inProgressResultsCorrectness = 'poprawność: ' + results.correctness.toFixed(1) + '%';
  var inProgressResultsSpeedContent = document.getElementById('in_progress_results_speed');
  inProgressResultsSpeedContent.innerHTML = inProgressResultsSpeed;
  var inProgressResultsCorrectnessContent = document.getElementById('in_progress_results_correctness');
  inProgressResultsCorrectnessContent.innerHTML = inProgressResultsCorrectness;

  var debugInfo = document.getElementById('debug_info');
  debugInfo.innerHTML = results.toHTMLString();
  setTimeout(this.updateInProgressResults.bind(this), 1000);
}

handlePlCharsCheckboxChange() {
  var checkbox = document.getElementById('plCharsCheckbox');
  this.plCharsOn = checkbox.checked;
  if (this.model.isReady && !this.model.isStarted) {
    this.model = new TypingTestModel(this.textWithPlChars, this.plCharsOn);
    this.draw();
  }
  this.canvas.focus();
}

handleNewTestButtonClick() {
  if (this.splashScreenVisible) {
    this.hideSplashScreen();
    return;
  }
  this.textWithPlChars = this.nextText();
  this.model = new TypingTestModel(this.textWithPlChars, this.plCharsOn);
  this.draw();
  this.canvas.focus();
}

nextText() {
  this.mockTextIndex = (this.mockTextIndex + 1) % this.mockTexts.length;
  return this.mockTexts[this.mockTextIndex];
}

hideSplashScreen() {
  this.splashScreenVisible = false;
  var splashScreen = document.getElementById('splash_screen');
  splashScreen.parentNode.removeChild(splashScreen);
  //this.canvas.focus();
}

showDialog() {
  var dialog = document.getElementById('dialog');
  dialog.style.display = 'table';
  var results = new TestResults(this.model);
  var dialogText = document.getElementById('dialog_results_text');
  dialogText.innerHTML = results.toHTMLString();
  var typingTest = document.getElementById('typing_test');
  typingTest.style.filter = 'blur(1px)';
}

hideDialog() {
  var dialog = document.getElementById('dialog');
  dialog.style.display = 'none';
  var typingTest = document.getElementById('typing_test');
  typingTest.style.filter = 'none';
}

submitTestResults() {
  var testResults = new TestResults(this.model);
  var params = new Object();
  params.speed = testResults.realSpeed.toFixed(1);
  params.mistakes = testResults.mistakesCount;
  params.corrections = testResults.correctionsCount;
  params.plChars = testResults.plChars;
  params.correctChars = testResults.writtenCharsCount - testResults.mistakesCount;
  params.minutes = parseInt(testResults.timeMinutes);
  params.seconds = parseInt(testResults.timeSeconds) % 60;
  params.timeVerifier = testResults.timeSecondsVerifier;
  params.h = this.h(this.hData + ':'
      + params.speed + ':'
      + params.mistakes + ':'
      + params.corrections + ':'
      + params.plChars + ':'
      + params.minutes + ':'
      + params.seconds + ':'
      + params.timeVerifier,
      "secret1");
  console.log('h for ttlog: ' + params.h);
}

h(hData, hKey) {
  return CryptoJS.HmacSHA1(hData, hKey);
}

preventBackspaceNavigation() {
  document.addEventListener('keydown', this.preventDefaultForBackspace.bind(this));
  document.addEventListener('keypress', this.preventDefaultForBackspace.bind(this));
}

preventDefaultForBackspace(e) {
  var rx = /INPUT|SELECT|TEXTAREA/i;
  if (e.which == 8) { // 8 == backspace
    if (!rx.test(e.target.tagName) || e.target.disabled || e.target.readOnly) {
      e.preventDefault();
    }
  }
}
}


    class TypingTestModel {

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


class TypingArea {

    constructor(context, width, height) {
        this.MAX_LINES = 12;
        this.MAX_VISIBLE_WRITTEN_LINES = 3;
        this.LINE_HEIGHT = 22;
        this.TOP_MARGIN_TEXT = 22;
        this.TOP_MARGIN_WRITTEN = 42;
        this.TEXT_TO_TYPE_COLOR = '#000000';
        this.WRITTEN_TEXT_COLOR = '#0000C0';
        this.MISTAKE_TEXT_COLOR = '#F00000';
        this.CORRECTED_TEXT_COLOR = '#C000D8';
        this.REGULAR_FONT = '15px Verdana';
        this.BOLD_FONT = 'bold ' + this.REGULAR_FONT;

        this.context = context;
        this.width = width;
        this.height = height;
        // text to type, not written by the user
        this.visibleTextLines = this.createVisibleTextLines();
    }

    draw(typingTestModel) {
        this.context.fillStyle = 'white';
        this.context.fillRect(0, 0, this.width, this.height)

        // TODO do we need the drawOnlyCurrentLine variable?
        const drawOnlyCurrentLine = typingTestModel.stayedInTheSameLine && false;
        const startLine = Math.max(
                typingTestModel.writtenLines.length
                    - this.MAX_VISIBLE_WRITTEN_LINES,
                0);
        const endLine = Math.min(
                startLine + this.MAX_VISIBLE_WRITTEN_LINES - 1,
                typingTestModel.writtenLines.length - 1);

        this.context.font = this.REGULAR_FONT;
        if (typingTestModel.isReady) {
            this.drawWrittenLines(typingTestModel, startLine, endLine, drawOnlyCurrentLine);
        }
        if (!drawOnlyCurrentLine) {
            this.drawTextLines(typingTestModel.textLines, startLine, endLine);
        }
    }

    createVisibleTextLines() {
        var visibleTextLines = [];
        for (var i = 0; i < this.MAX_LINES; i++) {
            var line = "";
            visibleTextLines.push(line);
        }
        return visibleTextLines;
    }

    drawTextLines(textLines,
            startLine, endLine) {
        var visibleIndex = 0;
        var i;
        for (i = startLine; i <= endLine; i++) {
            this.visibleTextLines[visibleIndex] = textLines[i];
            visibleIndex += 1;
            this.visibleTextLines[visibleIndex] = "";
            visibleIndex += 1;
        }
        for (i = endLine + 1; i < textLines.length
                && i < endLine + 1 + this.MAX_LINES
                    - 2 * (1 + endLine - startLine); i++) {
            this.visibleTextLines[visibleIndex] = textLines[i];
            visibleIndex += 1;
        }
        while (visibleIndex < this.visibleTextLines.length) {
            this.visibleTextLines[visibleIndex] = "";
            visibleIndex += 1;
        }

        this.context.fillStyle = this.TEXT_TO_TYPE_COLOR;
        var yShift = this.TOP_MARGIN_TEXT;
        for (var i = 0; i < this.MAX_LINES; i++) {
            this.context.fillText(this.visibleTextLines[i], 10, yShift);
            yShift += this.LINE_HEIGHT;
        }
    }

    drawWrittenLines(typingTestModel,
            startLine, endLine, drawOnlyCurrentLine) {
        this.context.fillStyle = this.WRITTEN_TEXT_COLOR;
        var yShift = this.TOP_MARGIN_WRITTEN;
        var visibleIndex = 0;
        var x = 10;
        for (var i = startLine; i <= endLine; i++) {
            if (!drawOnlyCurrentLine || i == endLine) {
                x = 10;
                for (var j = 0; j < typingTestModel.writtenLines[i].length; j++) {
                    var c = typingTestModel.writtenLines[i].charAt(j);
                    this.drawWrittenCharacter(c, x, yShift, i, j, typingTestModel);
                    x += this.context.measureText(c).width;
                }
            }
            visibleIndex += 1;
            yShift += 2 * this.LINE_HEIGHT;
        }
        yShift -= 2 * this.LINE_HEIGHT;
        const cursor = '_';
        this.context.fillStyle = typingTestModel.isMistakeMade ? this.MISTAKE_TEXT_COLOR : this.WRITTEN_TEXT_COLOR;
        this.context.fillText(cursor, x, yShift);
    }

    drawWrittenCharacter(c, x, yShift, i, j, typingTestModel) {
        if (typingTestModel.mistakes[i][j]) {
            this.context.fillStyle = this.MISTAKE_TEXT_COLOR;
            this.context.font = this.BOLD_FONT;
        } else if (typingTestModel.corrections[i][j]) {
            this.context.fillStyle = this.CORRECTED_TEXT_COLOR;
        } else {
            this.context.fillStyle = this.WRITTEN_TEXT_COLOR;
        }
        this.context.fillText(c, x, yShift);
        if (typingTestModel.mistakes[i][j]) {
            this.context.font = this.REGULAR_FONT;
        }
    }
}


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
        var utils = new Utils();
        this.plChars = typingTestModel.textLines.some(utils.containsPlChars.bind(utils));
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
        var html = "<span style=\"font-size: medium;\">prędkość: <b>"
            + this.realSpeed.toFixed(1) + "</b> znaków/min "
            + "(" + this.realSpeedWPM.toFixed(1) + " słów/min)<br>"
            + "poprawność: <b>" + this.correctness.toFixed(1) + "</b>%"
            + "</span><span style=\"font-size: small;\"><br><br>"
            + "Ilość błędów: " + (this.mistakesCount + this.correctionsCount);
        if (this.correctionsCount > 0 && this.mistakesCount == 0) {
            html += ", wszystkie poprawione.<br>";
        } else if (this.mistakesCount > 0) {
            html += ", z czego poprawiono " + this.correctionsCount + ".<br>";
        } else {
            html += ".<br>";
        }
        html += "Przepisano "
            + (this.writtenCharsCount - this.mistakesCount) + " znaków w czasie ";
        if (parseInt(this.timeMinutes) > 0) {
            html += parseInt(this.timeMinutes) + " min ";
        }
        html += (parseInt(this.timeSeconds) % 60) + " s." + "</span>";
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


    class Utils {

        constructor() {
            this.PL_TO_EN = {
                'ą': 'a', 'ć': 'c', 'ę': 'e', 'ł': 'l', 'ń': 'n', 'ó': 'o',
                'ś': 's', 'ż': 'z', 'ź': 'z', 'Ą': 'A', 'Ć': 'C', 'Ę': 'E',
                'Ł': 'L', 'Ń': 'N', 'Ó': 'O', 'Ś': 'S', 'Ż': 'Z', 'Ź': 'Z'
            };
        }

        breakLines(text, maxLineLength) /* of String */ {
            const multiSpace = / +/g;
            var textLines = text.replace(multiSpace, ' ').trim().split('\n');
            var lineEndIndex = 0;
            for (var i = 0; i < textLines.length; i++) {
                for (var j = 1; j < textLines[i].length
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

        shavePlChars(withPlChars) {
            var withoutPlChars = "";
            for (var i = 0; i < withPlChars.length; i++) {
                var c = withPlChars.charAt(i);
                withoutPlChars += c in this.PL_TO_EN ? this.PL_TO_EN[c] : c;
            }
            return withoutPlChars;
        }

        containsPlChars(s) {
            for (var i = 0; i < s.length; i++) {
                if (s.charAt(i) in this.PL_TO_EN) {
                    return true;
                }
            }
            return false;
        }
    }
