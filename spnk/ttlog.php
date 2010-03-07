<?
$speed = $_GET['speed'];
$mistakes = $_GET['mistakes'];
$pl = $_GET['plChars'];
$chars = $_GET['correctChars'];
$minutes = $_GET['minutes'];
$seconds = $_GET['seconds'];

// rejestruje tylko wyniki z nie wiecej niz 25 bledami
if (!empty($speed) && $mistakes <= 25) {
	mysql_connect();

	// konwersja polskiego u³amka dziesiêtnego (przecinek) na amerykañski (kropka)
	$speed = str_replace(",", ".", $speed);
	if (isset($_SERVER['HTTP_X_FORWARDED_FOR'])) {
		$ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
	} else {
		$ip = $_SERVER["REMOTE_ADDR"];
	}
	$query = "INSERT INTO `ttlog` (`date_added`, `ip`, `speed`, `mistakes`, `pl`,
		`chars`, `minutes`, `seconds`)
	VALUES(NOW(), '".$ip."', '".$speed."', '".$mistakes."', '".$pl."',
		'".$chars."', '".$minutes."', '".$seconds."')";
	mysql_query($query);

	mysql_close();
} else {
        echo "Does not compute.";
}
?>
