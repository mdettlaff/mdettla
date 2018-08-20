function say_hello() {
  return 'this is doge';
}

var context;
var canvas;
var model;
var typingArea;

function tt_init() {
  var content = document.getElementById('test_area');
  model = new TypingTestModel('foo bar', true);
  model.onPrintableChar('f')
  //model.onPrintableChar('x')
  model.onPrintableChar('o')
  model.onPrintableChar('o')
  model.onPrintableChar(' ')
  model.onPrintableChar('b')
  model.onPrintableChar('a')
  model.onPrintableChar('r')
  content.innerHTML = 'mistake: ' + model.isMistakeMade + ', is started: ' + model.isStarted + ', is finished: ' + model.isFinished;
  var utils = new Utils();
  var lines = utils.breakLines('foo bar baz', 8);
  var withoutPlChars = utils.shavePlChars('zażółć gęślą jaźń');
  var containsPlChars = utils.containsPlChars('zażółć');
  content.innerHTML += '; lines: ' + lines + ', withoutPlChars: ' + withoutPlChars + ', containsPlChars: ' + containsPlChars;
  content.innerHTML += '<br>mistake: false, is started: true, is finished: true; lines: foo bar,baz, withoutPlChars: zazolc gesla jazn, containsPlChars: true'

  canvas = document.getElementById("typing_area");
  context = canvas.getContext("2d");

  model = new TypingTestModel('W zeszły czwartek dwa rekiny ludojady pożarły osiemnastoletniego australijskiego surfera. Według świadków zdarzenia, rozerwały jego ciało na pół, a następnie spędziły parę minut walcząc o to, któremu z nich przypadnie który kawałek. Jak zwykle w takim przypadku, przeprowadzono wywiady z różnymi ekspertami od przyrody, którzy zgodnie stwierdzili, że rekiny te należy wypuścić na wolność po udzieleniu im pouczenia, częściowo dlatego, że są pod ochroną, a częściowo dlatego, że do takich ataków dochodzi niezwykle rzadko.', true);
  typingArea = new TypingArea(context, canvas.width, canvas.height);

  canvas.addEventListener('keydown', handleKeyPress);
  initContext();
  draw();

  updateInProgressResults();
}

function initContext() {
  context.font = "15px Verdana";
}

function draw() {
  context.clearRect(0, 0, canvas.width, canvas.height)
  typingArea.draw(model);
}

function handleKeyPress(event) {
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

function updateInProgressResults() {
  var results = new TestResults(model);
  var inProgressResults = 'prędkość: ' + results.realSpeed.toFixed(1) + ' znaków/min, ';
  inProgressResults += 'poprawność: ' + results.correctness.toFixed(1) + '%';
  var inProgressResultsContent = document.getElementById('in_progress_results');
  inProgressResultsContent.innerHTML = inProgressResults + '<br><br>' + results.toHTMLString();
  setTimeout(updateInProgressResults, 1000);
}

