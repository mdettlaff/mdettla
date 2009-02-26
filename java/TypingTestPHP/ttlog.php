<?
$typingSpeed = $_GET['speed'];
$mistakes = $_GET['mistakes'];
$correctChars = $_GET['correctChars'];
$minutes = $_GET['minutes'];
$seconds = $_GET['seconds'];

if (!empty($typingSpeed)) {
  $ttlog = fopen("ttlog.txt", "a");
  flock($ttlog, 2);
  fputs($ttlog, date("d-m-Y")." ".date("H:i").", ".$typingSpeed." zn./min, ".
    $mistakes." b³êdów (".$correctChars." znaków w ".$minutes." min ".
    $seconds." s)\n");
  flock($ttlog, 3);
  fclose($ttlog);
}
?>

