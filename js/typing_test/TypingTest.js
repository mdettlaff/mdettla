function say_hello() {
  return 'this is doge';
}

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

  var typingArea = document.getElementById("typing_area");
  var ctx = typingArea.getContext("2d");
  ctx.font = "18px Arial";
  ctx.fillText("Hello World", 10, 30);

  typingArea.addEventListener('keyup', handleKeyPress);
}

function handleKeyPress(event) {
  alert('key: ' + event.keyCode);
}

