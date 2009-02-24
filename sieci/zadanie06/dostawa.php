<HTML>
<HEAD>
<TITLE>Sklep - sieci komputerowe</TITLE>
</HEAD>
<BODY>
<h3>Dostawa towaru</h3>
<FORM ACTION="dostawa.php" METHOD=GET>
<?php
$dane=file("data/magazyn.txt");
$lines=count($dane);
$file=fopen("data/magazyn.txt", "w");
flock($file, 2);
// dodajemy towary z dostawy do magazynu
for($q=0; $q < $lines; $q++) {
  $info=explode("|", "$dane[$q]");
  if (!empty($_GET[$info[0]])) {
    fputs($file, $info[0]."|".$_GET[$info[0]]."|".$info[2]);
  } else {
    fputs($file, $info[0]."|".$info[1]."|".$info[2]);
  }
}
flock($file, 3);
fclose($file);

$dane=file("data/magazyn.txt");
$lines=count($dane);
for($q=0; $q < $lines; $q++) {
  $info=explode("|", "$dane[$q]");
  echo "<b>".$info[0].":</b> - ".$info[2]." z³<br>";
  echo "ilo¶æ w magazynie: ".$info[1]."<br>";
  echo "nowa ilo¶æ: <INPUT SIZE=2 TYPE=TEXT NAME=\"".$info[0]."\"><br>";
  echo "<br>";
}
?>
<INPUT TYPE=SUBMIT VALUE="Dostarcz towar">
</FORM>
</BODY>
</HTML>
