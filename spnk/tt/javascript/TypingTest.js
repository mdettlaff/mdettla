(function (typingTestAPI) {

typingTestAPI.init = function() {
	new TypingTest().init();
}

class TypingTest {

	constructor() {
		this.mockTexts = ['placeholder', 'Yes this is tak¿e pies, mo¿e jeszcze z jedn± linijk±.', 'jeszcze ³ubin'];
		this.mockTextIndex = -1;

		this.splashScreenVisible = true;
		this.plCharsOn = true;
		this.hData = '';
		this.typingTimeVerifierSeconds = 0;

		this.textWithPlChars = null;
		this.canvas = document.getElementById("typing_area");
		this.context = this.canvas.getContext("2d");
		this.utils = new Utils();
	}

	init() {
		this.model = new TypingTestModel('Trwa ³±czenie z serwerem, proszê czekaæ...', this.plCharsOn);
		this.typingArea = new TypingArea(this.context, this.canvas.width, this.canvas.height);
		this.draw();

		this.testResults = new TestResults(this.model);
		this.updateInProgressResults(this.testResults);
		this.updateInProgressResultsTicker();
		this.startTypingTimeVerifierTimer();
		this.addEventListeners();
		this.preventBackspaceAndSpaceNavigation();

		this.hideSplashScreen(); // hide splash screen for now to make testing easier

		this.sendRequestForNewText(false);
	}

	sendRequestForNewText(focusOnCanvas) {
		const inst = this;
		const xhr = new XMLHttpRequest();
		xhr.onreadystatechange = function() {
			if (this.readyState == 4 && this.status == 200) {
				inst.mockTextIndex = (inst.mockTextIndex + 1) % inst.mockTexts.length;
				if (inst.mockTextIndex == 0) {
					inst.textWithPlChars = this.responseXML.getElementsByTagName("text")[0].childNodes[0].nodeValue;
				} else {
					inst.textWithPlChars = inst.mockTexts[inst.mockTextIndex];
				}
				inst.hData = this.responseXML.getElementsByTagName("hData")[0].childNodes[0].nodeValue;
				inst.model = new TypingTestModel(inst.textWithPlChars, inst.plCharsOn);
				inst.updateInProgressResults(new TestResults(inst.model));
				inst.draw();
				if (focusOnCanvas) {
					inst.canvas.focus();
				}
			}
		};
		xhr.open('GET', 'tt/service/texts.php', true);
		xhr.responseType = 'document';
		xhr.send();
	}

	addEventListeners() {
		this.canvas.addEventListener('keydown', this.handleKeyPress.bind(this));
		document.getElementById('plCharsCheckbox').addEventListener('change', this.handlePlCharsCheckboxChange.bind(this));
		document.getElementById('newTestButton').addEventListener('click', this.handleNewTestButtonClick.bind(this));
		document.getElementById('splash_screen').addEventListener('click', this.hideSplashScreen.bind(this));
		document.getElementById('ok_button').addEventListener('click', this.hideDialog.bind(this));
		document.getElementById('cancel_button').addEventListener('click', this.hideDialog.bind(this));
		document.getElementById('ok_submit_button').addEventListener('click', this.handleSubmitButton.bind(this));
	}

	draw() {
		this.typingArea.draw(this.model);
	}

	handleKeyPress(e) {
		const wasStarted = this.model.isStarted;
		if (this.model.isReady && !this.model.isFinished && !this.model.isPaused) {
			if (e.keyCode == 8 /* backspace */) {
				this.model.onBackspace();
				this.draw();
			} else if (e.keyCode == 13 /* enter */) {
				this.model.onEnter();
				this.draw();
			} else if (e.keyCode >= 32 /* not a control character */
					&& e.key != null && e.key.length == 1) {
				var c = e.key;
				if (e.ctrlKey && e.altKey) {
					// polish chars require special treatment on Edge browser
					if (e.shiftKey) {
						c = this.utils.toPlCharUppercase(c);
					} else {
						c = this.utils.toPlChar(c);
					}
				}
				this.model.onPrintableChar(c);
				this.draw();
			}
			if (!wasStarted && this.model.isStarted) {
				this.typingTimeVerifierSeconds = 0;
			}
			if (this.model.isFinished) {
				this.handleTestFinished();
			}
		}
	}

	handleTestFinished() {
		this.testResults = new TestResults(this.model, this.typingTimeVerifierSeconds);
		this.updateInProgressResults(this.testResults);
		this.showDialog(this.testResults);
		this.submitTestResults(this.testResults);
		this.sendRequestForHighscoreRequiredSpeed(this.testResults);
	}

	sendRequestForHighscoreRequiredSpeed(testResults) {
		const inst = this;
		const xhr = new XMLHttpRequest();
		xhr.onreadystatechange = function() {
			if (this.readyState == 4 && this.status == 200) {
				const requiredSpeed = this.responseXML.getElementsByTagName("requiredSpeed")[0].childNodes[0].nodeValue;
				const usernameTagContent = this.responseXML.getElementsByTagName("username")[0].childNodes[0];
				const username = usernameTagContent != null ? usernameTagContent.nodeValue : null;
				inst.onHighscoreRequiredSpeedReceived.bind(inst)(requiredSpeed, username, testResults);
			}
		};
		xhr.open('GET', 'tt/service/highscore.php?q=get_threshold', true);
		xhr.responseType = 'document';
		xhr.send();
	}

	onHighscoreRequiredSpeedReceived(requiredSpeed, username, testResults) {
		if (testResults.realSpeed > Number(requiredSpeed)
				&& testResults.mistakesCount == 0
				&& testResults.plChars
				&& testResults.correctness > 95) {
			const usernameInput = document.getElementById('username_input');
			if (username != null && username != "") {
				usernameInput.value = username;
			}
			if (usernameInput.value == null || usernameInput.value == "") {
				document.getElementById('highscore_form').style.display = 'block';
				document.getElementById('ok_button_wrapper').style.display = 'none';
				usernameInput.focus();
			} else {
				document.getElementById('highscore_info').style.display = 'block';
				this.submitHighscore(testResults, usernameInput.value);
				updateHighscoreTable();
			}
		}
	}

	handleSubmitButton() {
		const usernameInput = document.getElementById('username_input');
		const username = usernameInput.value;
		if (username != null && username != "" && username.length >= 2 && username.length <= 32) {
			this.hideDialog();
			this.submitHighscore(this.testResults, username);
		} else {
			const validationMessage = document.getElementById('usernameValidationMessage');
			validationMessage.style.display = 'inline';
		}
	}

	updateInProgressResultsTicker() {
		if (this.model.isStarted && !this.model.isFinished) {
			const testResults = new TestResults(this.model);
			this.updateInProgressResults(testResults);
		}
		setTimeout(this.updateInProgressResultsTicker.bind(this), 1000);
	}

	startTypingTimeVerifierTimer() {
		setInterval(function() { this.typingTimeVerifierSeconds++; }.bind(this), 1000);
	}

	updateInProgressResults(testResults) {
		var inProgressResultsSpeed = 'prêdko¶æ: ' + testResults.realSpeed.toFixed(1) + ' znaków/min';
		var inProgressResultsCorrectness = 'poprawno¶æ: ' + testResults.correctness.toFixed(1) + '%';
		var inProgressResultsSpeedContent = document.getElementById('in_progress_results_speed');
		inProgressResultsSpeedContent.innerHTML = inProgressResultsSpeed;
		var inProgressResultsCorrectnessContent = document.getElementById('in_progress_results_correctness');
		inProgressResultsCorrectnessContent.innerHTML = inProgressResultsCorrectness;
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
		this.sendRequestForNewText(true);
	}

	hideSplashScreen() {
		this.splashScreenVisible = false;
		var splashScreen = document.getElementById('splash_screen');
		splashScreen.parentNode.removeChild(splashScreen);
		//this.canvas.focus();
	}

	showDialog(testResults) {
		document.getElementById('highscore_form').style.display = 'none';
		document.getElementById('highscore_info').style.display = 'none';
		document.getElementById('ok_button_wrapper').style.display = 'block';
		var dialog = document.getElementById('dialog');
		dialog.style.display = 'table';
		var dialogText = document.getElementById('dialog_results_text');
		dialogText.innerHTML = testResults.toHTMLString();
		var typingTest = document.getElementById('typing_test');
		typingTest.style.filter = 'blur(1px)';
	}

	hideDialog() {
		var dialog = document.getElementById('dialog');
		dialog.style.display = 'none';
		var typingTest = document.getElementById('typing_test');
		typingTest.style.filter = 'none';
	}

	submitTestResults(testResults) {
		var params = new Object();
		params.speed = testResults.realSpeed.toFixed(1);
		params.mistakes = testResults.mistakesCount;
		params.corrections = testResults.correctionsCount;
		params.plChars = testResults.plChars;
		params.correctChars = testResults.writtenCharsCount - testResults.mistakesCount;
		params.minutes = parseInt(testResults.timeMinutes);
		params.seconds = parseInt(testResults.timeSeconds) % 60;
		params.timeVerifier = testResults.timeSecondsVerifier;
		const hInput = this.hData + ':'
				+ params.speed + ':'
				+ params.mistakes + ':'
				+ params.corrections + ':'
				+ params.plChars + ':'
				+ params.minutes + ':'
				+ params.seconds + ':'
				+ params.timeVerifier;
		params.h = this.h(hInput, "secret1");
		var formData = new FormData();
		formData.append('speed', params.speed);
		formData.append('mistakes', params.mistakes);
		formData.append('corrections', params.corrections);
		formData.append('plChars', params.plChars);
		formData.append('correctChars', params.correctChars);
		formData.append('minutes', params.minutes);
		formData.append('seconds', params.seconds);
		formData.append('timeVerifier', params.timeVerifier);
		formData.append('h', params.h);
		const xhr = new XMLHttpRequest();
		xhr.open('POST', 'tt/service/ttlog.php', true);
		xhr.send(formData);
		console.log('h for ttlog: ' + params.h + ', h input: ' + hInput);
	}

	submitHighscore(testResults, username) {
		var params = new Object();
		params.username = username;
		params.speed = testResults.realSpeed.toFixed(1);
		params.mistakes = testResults.mistakesCount;
		params.corrections = testResults.correctionsCount;
		params.plChars = testResults.plChars;
		params.correctChars = testResults.writtenCharsCount - testResults.mistakesCount;
		params.minutes = parseInt(testResults.timeMinutes);
		params.seconds = parseInt(testResults.timeSeconds) % 60;
		params.timeVerifier = testResults.timeSecondsVerifier;
		const hInput = this.hData + ':'
				+ params.speed + ':'
				+ params.mistakes + ':'
				+ params.corrections + ':'
				+ params.plChars + ':'
				+ params.minutes + ':'
				+ params.seconds + ':'
				+ params.timeVerifier;
		params.h = this.h(hInput, "secret2");
		var formData = new FormData();
		formData.append('username', params.username);
		formData.append('speed', params.speed);
		formData.append('mistakes', params.mistakes);
		formData.append('corrections', params.corrections);
		formData.append('plChars', params.plChars);
		formData.append('correctChars', params.correctChars);
		formData.append('minutes', params.minutes);
		formData.append('seconds', params.seconds);
		formData.append('timeVerifier', params.timeVerifier);
		formData.append('h', params.h);
		const xhr = new XMLHttpRequest();
		xhr.open('POST', 'data:text/html;,', true);
		xhr.send(formData);
		console.log('h for highscore: ' + params.h + ', h input: ' + hInput);
	}

	h(hData, hKey) {
		return CryptoJS.HmacSHA1(hData, hKey);
	}

	preventBackspaceAndSpaceNavigation() {
		document.addEventListener('keydown', this.preventDefaultForBackspaceAndSpace.bind(this));
		document.addEventListener('keypress', this.preventDefaultForBackspaceAndSpace.bind(this));
	}

	preventDefaultForBackspaceAndSpace(e) {
		var rx = /INPUT|SELECT|TEXTAREA/i;
		if (e.which == 8 || e.keyCode == 32) { // 8 == backspace
			if (!rx.test(e.target.tagName) || e.target.disabled || e.target.readOnly) {
				e.preventDefault();
			}
		}
	}
}


class TypingTestModel {

	constructor(text,
			plCharsOn, isReady = true) {
		this.MAX_LINE_LENGTH = 66;
		this.isReady = isReady;
		this.stayedInTheSameLine = false;
		const utils = new Utils();
		if (!plCharsOn) {
			text = utils.shavePlChars(text);
		}
		this.textLines = utils.breakLines(text, this.MAX_LINE_LENGTH);
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
		var html = "<span style=\"font-size: medium;\">prêdko¶æ: <b>"
			+ this.realSpeed.toFixed(1) + "</b> znaków/min "
			+ "(" + this.realSpeedWPM.toFixed(1) + " s³ów/min)<br>"
			+ "poprawno¶æ: <b>" + this.correctness.toFixed(1) + "</b>%"
			+ "</span><span style=\"font-size: small;\"><br><br>"
			+ "Ilo¶æ b³êdów: " + (this.mistakesCount + this.correctionsCount);
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
			'±': 'a', 'æ': 'c', 'ê': 'e', '³': 'l', 'ñ': 'n', 'ó': 'o',
			'¶': 's', '¿': 'z', '¼': 'z', '¡': 'A', 'Æ': 'C', 'Ê': 'E',
			'£': 'L', 'Ñ': 'N', 'Ó': 'O', '¦': 'S', '¯': 'Z', '¬': 'Z'
		};
		this.EN_TO_PL_LOWER = {
			'a': '±', 'c': 'æ', 'e': 'ê', 'l': '³', 'n': 'ñ', 'o': 'ó',
			's': '¶', 'z': '¿', 'x': '¼'
		};
		this.EN_TO_PL_UPPER = {
			'a': '¡', 'c': 'Æ', 'e': 'Ê', 'l': '£', 'n': 'Ñ', 'o': 'Ó',
			's': '¦', 'z': '¯', 'x': '¬'
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

	toPlChar(c) {
		return c in this.EN_TO_PL_LOWER ? this.EN_TO_PL_LOWER[c] : c;
	}

	toPlCharUppercase(c) {
		return c in this.EN_TO_PL_UPPER ? this.EN_TO_PL_UPPER[c] : c;
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

} (window.typingTestAPI = window.typingTestAPI || {}));
