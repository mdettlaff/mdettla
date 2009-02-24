<?
// inicjalizujemy obrazek i kolory
Header("Content-Type: image/png");
$im = ImageCreate(151, 151);

$white = ImageColorAllocate($im, 250, 250, 250);
$black = ImageColorAllocate($im, 0, 0, 0);
//$red = ImageColorAllocate($im, 255, 50, 50);

$SQ = 50; // rozmiar pola planszy
$BS = 3; // rozmiar planszy

ImageFill($im, 0, 0, $white); // wype³niamy ca³y obrazek bia³ym kolorem

// rysujemy linie siatki
for ($i=0; $i <= $BS; $i++) { // pionowe
  ImageLine($im, $i*$SQ, 0, $i*$SQ, $BS*$SQ, $black);
}
for ($i=0; $i <= $BS; $i++) { // poziome
  ImageLine($im, 0, $i*$SQ, $BS*$SQ, $i*$SQ, $black);
}

// odczytujemy planszê z pliku
$dane=file("data/board.txt");
$ilosc=count($dane);
for($q=0; $q < $ilosc; $q++) {
  $info=explode("|", "$dane[$q]");
  $r = 0;
  foreach ($info as $kolor_pola) {
    if (!empty($kolor_pola)) {
      $board[$r][$q] = $kolor_pola;
    } else {
      $board[$r][$q] = " ";
    }
    $r++;
  }
}
// rysujemy kulasy na planszy
for ($i=0; $i < $BS; $i++) {
  for ($j=0; $j < $BS; $j++) {
    if ($board[$i][$j] == "x") {
      ImageLine($im, $i*$SQ+5, $j*$SQ+5, ($i+1)*$SQ-5, ($j+1)*$SQ-5, $black);
      ImageLine($im, ($i+1)*$SQ-5, $j*$SQ+5, $i*$SQ+5, ($j+1)*$SQ-5, $black);
    } else if ($board[$i][$j] == "o") {
      ImageEllipse($im, $i*$SQ+25, $j*$SQ+25, $SQ-7, $SQ-7, $black);
    }
  }
}


//ImageLine($im, 150, 150, 225, 150, $black);
//ImageString($im,3, 10, 10, "Gracz 1: 0 punktów", $black);
//ImageFilledRectangle($im, 10, 80, 30, 50, $blue);
//ImageFillToBorder($im, $mid_x, $mid_y, $black, $WedgeColor);		

// wypisanie obrazka w przegl±darce
ImagePNG($im);
?>
