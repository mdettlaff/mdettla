<?php if (empty($_COOKIE["player"])) { setcookie("player", rand(), time()+3600*3); } ?>
<html>
<head>
  <META HTTP-EQUIV="Content-type" CONTENT="text/html; charset=iso-8859-2">
  <title>Witaj!</title>
</head>
<body>
<h3>Gomoku</h3>
<a href="game.php">Kliknij tutaj, aby rozpocząć grę</a>
</body>
</html>
