function say_hello() {
  return 'this is doge';
}

function tt_init() {
  var content = document.getElementById('typing_area');
  content.innerHTML = 'text to type: ' + say_hello();
}

