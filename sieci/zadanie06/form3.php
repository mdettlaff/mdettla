<?php if (!isset($_COOKIE["id"])) setcookie("id", rand()); ?>
<?php //if (!isset($_COOKIE["id"])) header('Location: form1.php'); ?>
<HTML>
<HEAD>
<TITLE>Sklep - sieci komputerowe</TITLE>
</HEAD>
<BODY>
<?php
$is_enough = true;
$dane=file("data/magazyn.txt");
$lines=count($dane);
for($q=0; $q < $lines; $q++) {
  $info=explode("|", "$dane[$q]");
  if (!empty($_COOKIE[$info[0]])) {
    if ($_COOKIE[$info[0]] > $info[1]) {
      $is_enough = false;
    }
  }
}
if ($is_enough) {
  echo "<h3>Transakcja dokonana!</h3>";
  echo "Twoje dane:<br>";
  echo "Imiê: ".$_POST["private_imie"]."<br>";
  echo "Nazwisko: ".$_POST["private_nazwisko"]."<br>";
  echo "Adres: ".$_POST["private_adres"]."<br>";
  echo "NIP: ".$_POST["private_nip"]."<br>";

  echo "<br>Dokonane zakupy:<br>";
  foreach ($_COOKIE as $key => $value) {
    if (!empty($value) && $key != "id") {
      echo $key.": ".$value." szt.<br>";
    }
  }
  // zapisujemy transakcjê w pliku
  $file=fopen("data/transakcje.txt", "a");
  flock($file, 2);
  $record = $_SERVER['REMOTE_ADDR']."|".date("Y.m.d")."|"
    .$_POST["private_imie"]."|".$_POST["private_nazwisko"]."|"
    .$_POST["private_adres"]."|".$_POST["private_nip"];
  foreach ($_COOKIE as $key => $value) {
    if (!empty($value) && $key != "id") {
      $record = $record."|".$key."x".$value;
    }
  }
  if (isset($_COOKIE["id"])) {
    $record = $record."|discount";
  }
  fputs($file, $record."\n");
  flock($file, 3);
  fclose($file);
  // usuwamy kupione towary z magazynu
  $dane=file("data/magazyn.txt");
  $lines=count($dane);
  $file=fopen("data/magazyn.txt", "w");
  flock($file, 2);
  for($q=0; $q < $lines; $q++) {
    $info=explode("|", "$dane[$q]");
    if (!empty($_COOKIE[$info[0]])) {
      fputs($file, $info[0]."|".($info[1]-$_COOKIE[$info[0]])."|".$info[2]);
    } else {
      fputs($file, $info[0]."|".$info[1]."|".$info[2]);
    }
  }
  flock($file, 3);
  fclose($file);
} else {
  echo "W magazynie brakuje podanych towarów<br>";
}
echo "<br><a href=\"form1.php\">Powrót do strony g³ównej</a>";
?>
</BODY>
</HTML>
