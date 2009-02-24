<html>
<body>
Dzisiaj jest:<br>
<? echo date(r)."<br>";
   echo date(c)."<br>";
   echo date(U); ?>
<br><br>
<?php $x=5; ?>
Oto podstawiona warto¶æ:<br>
<?php echo $x; ?>
<br><br>
Oto uruchomiony program (jak nic nie widaæ, to widocznie admini zabronili):<br>
<?php exec("/home/inf/mdettla/public_html/sieci/lab05/hello"); ?>
</body>
</html>
