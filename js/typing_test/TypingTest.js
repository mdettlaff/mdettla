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
