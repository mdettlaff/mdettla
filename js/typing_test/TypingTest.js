function say_hello() {
  return 'this is doge';
}

var context;
var canvas;
var typedText = '';

function tt_init() {
  var content = document.getElementById('test_area');
  var model = new TypingTestModel('foo bar', true);
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

  canvas = document.getElementById("typing_area");
  context = canvas.getContext("2d");

  canvas.addEventListener('keyup', handleKeyPress);
  draw();
}

function draw() {
  context.clearRect(0, 0, canvas.width, canvas.height)
  drawText();
}

function drawText() {
  context.font = "18px Arial";
  context.fillStyle = 'black';
  context.fillText("Hello World", 10, 30);
  context.fillStyle = 'blue';
  context.fillText(typedText, 10, 52);
}

function handleKeyPress(event) {
  if (event.keyCode >= 32) { // not a control character
    typedText += event.key;
  }
  draw();
}

