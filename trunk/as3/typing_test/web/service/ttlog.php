<?php

include '../include/db_connection.php';

$speed = pg_escape_string($_POST['speed']);
$mistakes = pg_escape_string($_POST['mistakes']);
$pl = pg_escape_string($_POST['plChars']);
$chars = pg_escape_string($_POST['correctChars']);
$minutes = pg_escape_string($_POST['minutes']);
$seconds = pg_escape_string($_POST['seconds']);

// rejestruje tylko wyniki z nie wiêcej ni¿ 25 b³êdami
if (!empty($speed) && $mistakes <= 25) {
    // konwersja polskiego u³amka dziesiêtnego (przecinek)
    // na amerykañski (kropka)
    $speed = str_replace(',', '.', $speed);
    if (isset($_SERVER['HTTP_X_FORWARDED_FOR'])) {
        $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
    } else {
        $ip = $_SERVER['REMOTE_ADDR'];
    }
    $query = "
        INSERT INTO ttlog
            (date_added, ip, speed, mistakes,
                pl, chars, minutes, seconds)
            VALUES
            (NOW(), '$ip', '$speed', '$mistakes',
                '$pl', '$chars', '$minutes', '$seconds')
    ";
    pg_query($query);
} else {
    echo 'Does not compute.';
}

?>
