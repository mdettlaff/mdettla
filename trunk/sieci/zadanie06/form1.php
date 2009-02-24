<HTML>
<HEAD>
<!-- <META HTTP-EQUIV=Refresh CONTENT="2; URL=form1.php"> -->
<TITLE>Sklep - sieci komputerowe</TITLE>
</HEAD>
<BODY>
<h3>Sklep internetowy</h3>
<FORM ACTION="form2.php" METHOD=GET>
<?php
$dane=file("data/magazyn.txt");
$lines=count($dane);
for($q=0; $q < $lines; $q++) {
  $info=explode("|", "$dane[$q]");
  echo "<b>".$info[0].":</b> - ".$info[2]." z³<br>";
  echo "ilo¶æ do zakupu: <INPUT SIZE=2 TYPE=TEXT NAME=\"".$info[0]."\"> ".
    $info[3]."<br>";
  echo "ilo¶æ w magazynie: ".$info[1]."<br>";
  echo "<br>";
}
?>
<INPUT TYPE=SUBMIT VALUE="Kontynuacja zakupów">
</FORM>
</BODY>
</HTML>
