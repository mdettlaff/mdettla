<?php
foreach ($_GET as $key => $value)
  setcookie($key, $value, time()+3600);
//if (!isset($_COOKIE["id"])) header('Location: form1.php');
?>
<HTML>
<HEAD>
<TITLE>Sklep - sieci komputerowe</TITLE>
</HEAD>
<BODY>
<h3>Formularz zamówienia</h3>
<?php
$sum = 0;
// sprawdzamy czy wystarczajaca ilosc towaru jest w magazynie
$is_enough = true;
$is_cart_empty = true;
$dane=file("data/magazyn.txt");
$lines=count($dane);
for($q=0; $q < $lines; $q++) {
  $info=explode("|", "$dane[$q]");
  if (!empty($_GET[$info[0]])) {
    $sum = $sum + $_GET[$info[0]]*$info[2];
    if ($_GET[$info[0]] > $info[1]) {
      $is_enough = false;
      echo "Niewystarczaj±ca ilo¶æ produktu ".$info[0]." w magazynie";
      echo " (dostêpnych jest ".$info[1]." egzemplarzy).<br>";
    }
  }
}
foreach ($_GET as $value) {
  if (!empty($value)) $is_cart_empty = false;
}
if ($is_enough && !$is_cart_empty) {
  echo "<FORM ACTION=\"form3.php\" METHOD=POST>";
  echo "<table><tr><td align=right>Imiê:<br>Nazwisko:<br>Adres:<br>NIP:</td><td>";
  echo "  <INPUT TYPE=TEXT NAME=\"private_imie\"><br>";
  echo "  <INPUT TYPE=TEXT NAME=\"private_nazwisko\"><br>";
  echo "  <INPUT TYPE=TEXT NAME=\"private_adres\"><br>";
  echo "  <INPUT TYPE=TEXT NAME=\"private_nip\">";
  echo "</td></tr></table>";
  echo "<INPUT TYPE=SUBMIT VALUE=\"Zamów\">";
  echo "</FORM>";
  echo "<br>Zawarto¶æ koszyka:<br><br>";
  foreach ($_GET as $key => $value) {
    if (!empty($value)) {
      echo $key.": ".$value."<br>";
    }
  }
  if (isset($_COOKIE["id"])) {
    echo "<br>dostajesz zni¿kê 10%!";
    $sum = $sum * 0.9;
  }
  echo "<br>Ca³kowita kwota zakupu: ".$sum." z³<br>";
} else if ($is_cart_empty) {
  echo "Koszyk jest pusty - musisz wybraæ jakie¶ towary<br>";
}
echo "<br><a href=\"form1.php\">Powrót</a>";
?>
</BODY>
</HTML>
