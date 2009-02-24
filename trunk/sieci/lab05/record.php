<html>
<body>
Ta strona zbiera informacje o tobie, mimo iz nic o tym nie wiesz.
<br><br>
<?php
$wizyty=fopen("/home/inf/mdettla/public_html/sieci/lab05/visits.txt", "a");
flock($wizyty, 2);
fputs($wizyty, date("Y.m.d")." ".date("H:i")."|".
    gethostbyaddr($_SERVER['REMOTE_ADDR'])."\n");
flock($wizyty, 3);
fclose($wizyty);
?>
</body>
</html>
