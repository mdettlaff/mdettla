<html>
<body>
Tabliczka mno¿enia o rozmiarach
<?php
$k = $_GET["k"];
$n = $_GET["n"];
?>
<?php echo $k ?> wierszy na
<?php echo $n ?> kolumn.
<br><br>
<?php
echo "<table border=\"2\" cellpadding=\"2\">";
for ($i=1; $i <= $k; $i++) {
  echo "<tr>";
  for ($j=1; $j <= $n; $j++) {
    $red = (pow($i,7)*pow($j,3)) % 50 + 200;
    $green = (pow($i,4)*pow($j,9)) % 50 + 200;
    $blue = (pow($i,5)*pow($j,6)) % 50 + 200;
    echo "<td bgcolor=\"#".rgb2html($red,$green,$blue)."\">".$i*$j."</td>";
  }
  echo "</tr>";
}
echo "</table>";

function rgb2html($r, $g=-1, $b=-1) {
  if (is_array($r) && sizeof($r) == 3)
    list($r, $g, $b) = $r;

  $r = intval($r); $g = intval($g);
  $b = intval($b);

  $r = dechex($r<0?0:($r>255?255:$r));
  $g = dechex($g<0?0:($g>255?255:$g));
  $b = dechex($b<0?0:($b>255?255:$b));

  $color = (strlen($r) < 2?'0':'').$r;
  $color .= (strlen($g) < 2?'0':'').$g;
  $color .= (strlen($b) < 2?'0':'').$b;
  return '#'.$color;
}
?>
</body>
</html>
