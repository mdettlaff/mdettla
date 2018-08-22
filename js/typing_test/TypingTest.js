var context;
var canvas;
var model;
var typingArea;
var textWithPlChars;
var plCharsOn = true;
var mockTexts = ['W zeszły czwartek dwa rekiny ludojady pożarły osiemnastoletniego australijskiego surfera. Według świadków zdarzenia, rozerwały jego ciało na pół, a następnie spędziły parę minut walcząc o to, któremu z nich przypadnie który kawałek. Jak zwykle w takim przypadku, przeprowadzono wywiady z różnymi ekspertami od przyrody, którzy zgodnie stwierdzili, że rekiny te należy wypuścić na wolność po udzieleniu im pouczenia, częściowo dlatego, że są pod ochroną, a częściowo dlatego, że do takich ataków dochodzi niezwykle rzadko.', 'Yes this is także pies, może jeszcze z jedną linijką.', 'jeszcze jeden'];
var mockTextIndex = -1;

function tt_init() {
  canvas = document.getElementById("typing_area");
  context = canvas.getContext("2d");

  textWithPlChars = nextText();
  model = new TypingTestModel(textWithPlChars, plCharsOn);
  typingArea = new TypingArea(context, canvas.width, canvas.height);

  initContext();
  draw();

  updateInProgressResults();

  canvas.addEventListener('keydown', handleKeyPress);
  var plCharsCheckbox = document.getElementById('plCharsCheckbox');
  plCharsCheckbox.addEventListener('change', handlePlCharsCheckboxChange);
  var newTestButton = document.getElementById('newTestButton');
  newTestButton.addEventListener('click', handleNewTestButtonClick);
}

function initContext() {
  context.font = "15px Verdana";
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
    } else if (event.keyCode >= 32 /* not a control character */) {
      model.onPrintableChar(event.key);
      draw();
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
  textWithPlChars = nextText();
  model = new TypingTestModel(textWithPlChars, plCharsOn);
  draw();
  canvas.focus();
}

function nextText() {
  mockTextIndex = (mockTextIndex + 1) % mockTexts.length;
  return mockTexts[mockTextIndex];
}

