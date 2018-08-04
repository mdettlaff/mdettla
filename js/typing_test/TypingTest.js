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

  model = new TypingTestModel('foo Hello World bar', true);

  canvas.addEventListener('keydown', handleKeyPress);
  initContext();
  draw();
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
  context.fillText(model.textLines[0], 10, 30);
  context.fillStyle = 'blue';
  context.fillText(model.writtenLines[0] + '_', 10, 52);
}

function handleKeyPress(event) {
  if (event.keyCode == 8 /* backspace */) {
    model.onBackspace();
    draw();
  } else if (event.keyCode >= 32) { // not a control character
    model.onPrintableChar(event.key);
    draw();
  }
}

