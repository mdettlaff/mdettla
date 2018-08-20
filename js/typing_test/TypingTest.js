function say_hello() {
  return 'this is doge';
}

var context;
var canvas;
var model;

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

  canvas.addEventListener('keydown', handleKeyPress);
  initContext();
  draw();

  updateInProgressResults();
}

function initContext() {
  context.font = "18px Arial";
}

function draw() {
  context.clearRect(0, 0, canvas.width, canvas.height)
  drawText();
}

function drawText() {
  context.fillStyle = 'black';
  var verticalOffset = 55;
  for (var i = 0; i < model.textLines.length; i++) {
    context.fillText(model.textLines[i], 10, 30 + i * verticalOffset);
  }
  context.fillStyle = 'blue';
  for (var i = 0; i < model.writtenLines.length; i++) {
    var x = 10;
    for (var j = 0; j < model.writtenLines[i].length; j++) {
      var c = model.writtenLines[i][j];
      drawWrittenCharacter(c, x, verticalOffset, i, j);
      x += context.measureText(c).width;
    }
    context.fillStyle = model.isMistakeMade ? 'red' : 'blue';
    context.fillText('_', x, 52 + i * verticalOffset);
  }
}

function drawWrittenCharacter(c, x, verticalOffset, i, j) {
  if (model.mistakes[i][j]) {
    context.fillStyle = 'red';
  } else if (model.corrections[i][j]) {
    context.fillStyle = 'purple';
  } else {
    context.fillStyle = 'blue';
  }
  context.fillText(c, x, 52 + i * verticalOffset);
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

