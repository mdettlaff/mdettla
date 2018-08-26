var context;
var canvas;
var model;
var typingArea;
var textWithPlChars;
var plCharsOn = true;
var splashScreenVisible = true;
var hData = '';
var mockTexts = ['W zeszły czwartek dwa rekiny ludojady pożarły osiemnastoletniego australijskiego surfera. Według świadków zdarzenia, rozerwały jego ciało na pół, a następnie spędziły parę minut walcząc o to, któremu z nich przypadnie który kawałek. Jak zwykle w takim przypadku, przeprowadzono wywiady z różnymi ekspertami od przyrody, którzy zgodnie stwierdzili, że rekiny te należy wypuścić na wolność po udzieleniu im pouczenia, częściowo dlatego, że są pod ochroną, a częściowo dlatego, że do takich ataków dochodzi niezwykle rzadko.', 'Yes this is także pies, może jeszcze z jedną linijką.', 'jeszcze jeden'];
var mockTextIndex = -1;

function tt_init() {
  canvas = document.getElementById("typing_area");
  context = canvas.getContext("2d");

  textWithPlChars = nextText();
  model = new TypingTestModel(textWithPlChars, plCharsOn);
  typingArea = new TypingArea(context, canvas.width, canvas.height);

  draw();

  updateInProgressResults();

  canvas.addEventListener('keydown', handleKeyPress);
  var plCharsCheckbox = document.getElementById('plCharsCheckbox');
  plCharsCheckbox.addEventListener('change', handlePlCharsCheckboxChange);
  var newTestButton = document.getElementById('newTestButton');
  newTestButton.addEventListener('click', handleNewTestButtonClick);
  var splashScreen = document.getElementById('splash_screen');
  splashScreen.addEventListener('click', hideSplashScreen);
  var okButton = document.getElementById('ok_button');
  okButton.addEventListener('click', hideDialog);

  preventBackspaceNavigation();

  hideSplashScreen(); // hide splash screen for now to make testing easier
}

function draw() {
  context.fillStyle = 'white';
  context.fillRect(0, 0, canvas.width, canvas.height)
  typingArea.draw(model);
}

function handleKeyPress(event) {
  if (model.isReady && !model.isFinished && !model.isPaused) {
    if (event.keyCode == 8 /* backspace */) {
      model.onBackspace();
      draw();
    } else if (event.keyCode == 13 /* enter */) {
      model.onEnter();
      draw();
    } else if (event.keyCode >= 32 /* not a control character */
        && event.key != null && event.key.length == 1) {
      model.onPrintableChar(event.key);
      draw();
    }
    if (model.isFinished) {
      showDialog();
      submitTestResults();
    }
  }
}

function updateInProgressResults() {
  var results = new TestResults(model);
  var inProgressResultsSpeed = 'prędkość: ' + results.realSpeed.toFixed(1) + ' znaków/min';
  var inProgressResultsCorrectness = 'poprawność: ' + results.correctness.toFixed(1) + '%';
  var inProgressResultsSpeedContent = document.getElementById('in_progress_results_speed');
  inProgressResultsSpeedContent.innerHTML = inProgressResultsSpeed;
  var inProgressResultsCorrectnessContent = document.getElementById('in_progress_results_correctness');
  inProgressResultsCorrectnessContent.innerHTML = inProgressResultsCorrectness;

  var debugInfo = document.getElementById('debug_info');
  debugInfo.innerHTML = results.toHTMLString();
  setTimeout(updateInProgressResults, 1000);
}

function handlePlCharsCheckboxChange() {
  plCharsOn = this.checked;
  if (model.isReady && !model.isStarted) {
    model = new TypingTestModel(textWithPlChars, plCharsOn);
    draw();
  }
}

function handleNewTestButtonClick() {
  if (splashScreenVisible) {
    hideSplashScreen();
    return;
  }
  textWithPlChars = nextText();
  model = new TypingTestModel(textWithPlChars, plCharsOn);
  draw();
  canvas.focus();
}

function nextText() {
  mockTextIndex = (mockTextIndex + 1) % mockTexts.length;
  return mockTexts[mockTextIndex];
}

function hideSplashScreen() {
  splashScreenVisible = false;
  var splashScreen = document.getElementById('splash_screen');
  splashScreen.parentNode.removeChild(splashScreen);
  //canvas.focus();
}

function showDialog() {
  var dialog = document.getElementById('dialog');
  dialog.style.display = 'table';
  var results = new TestResults(model);
  var dialogText = document.getElementById('dialog_results_text');
  dialogText.innerHTML = results.toHTMLString();
  var typingTest = document.getElementById('typing_test');
  typingTest.style.filter = 'blur(1px)';
}

function hideDialog() {
  var dialog = document.getElementById('dialog');
  dialog.style.display = 'none';
  var typingTest = document.getElementById('typing_test');
  typingTest.style.filter = 'none';
}

function submitTestResults() {
  var testResults = new TestResults(model);
  var params = new Object();
  params.speed = testResults.realSpeed.toFixed(1);
  params.mistakes = testResults.mistakesCount;
  params.corrections = testResults.correctionsCount;
  params.plChars = testResults.plChars;
  params.correctChars = testResults.writtenCharsCount - testResults.mistakesCount;
  params.minutes = parseInt(testResults.timeMinutes);
  params.seconds = parseInt(testResults.timeSeconds) % 60;
  params.timeVerifier = testResults.timeSecondsVerifier;
  params.h = h(hData + ':'
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

function h(hData, hKey) {
  return CryptoJS.HmacSHA1(hData, hKey);
}

function preventBackspaceNavigation() {
  document.addEventListener('keydown', preventDefaultForBackspace);
  document.addEventListener('keypress', preventDefaultForBackspace);
}

function preventDefaultForBackspace(e) {
  var rx = /INPUT|SELECT|TEXTAREA/i;
  if (e.which == 8) { // 8 == backspace
    if (!rx.test(e.target.tagName) || e.target.disabled || e.target.readOnly) {
      e.preventDefault();
    }
  }
}

