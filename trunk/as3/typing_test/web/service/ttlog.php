<?php

include '../include/db_connection.php';
include '../include/log.php';

function validate($speed, $mistakes, $pl, $chars, $minutes, $seconds) {
    return is_numeric(str_replace(',', '.', $speed)) && is_numeric($mistakes)
        && ($pl == 'true' || $pl == 'false') && is_numeric($chars)
        && is_numeric($minutes) && is_numeric($seconds) && $mistakes <= 25;
}

$speed = $_POST['speed'];
$mistakes = $_POST['mistakes'];
$pl = $_POST['plChars'];
$chars = $_POST['correctChars'];
$minutes = $_POST['minutes'];
$seconds = $_POST['seconds'];

// rejestruje tylko wyniki z nie wiêcej ni¿ 25 b³êdami
if (validate($speed, $mistakes, $pl, $chars, $minutes, $seconds)) {
    // konwersja polskiego u³amka dziesiêtnego (przecinek)
    // na amerykañski (kropka)
    $speed = str_replace(',', '.', $speed);
    if (isset($_SERVER['HTTP_X_FORWARDED_FOR'])) {
        $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
    } else {
        $ip = $_SERVER['REMOTE_ADDR'];
    }
    $speed = pg_escape_string($speed);
    $mistakes = pg_escape_string($mistakes);
    $pl = pg_escape_string($pl);
    $chars = pg_escape_string($chars);
    $minutes = pg_escape_string($minutes);
    $seconds = pg_escape_string($seconds);
    $query = "
        INSERT INTO tt.ttlog
            (date_added, ip, speed, mistakes,
                pl, chars, minutes, seconds)
            VALUES
            (NOW(), '$ip', $speed, $mistakes,
                '$pl', $chars, $minutes, $seconds)
    ";
    pg_query($query) or log_write("ERROR: Problem with query: $query\n"
        . pg_last_error());
} else {
    echo 'Does not compute.';
    log_write("entry not added to ttlog; parameters: speed=$speed, "
        . "mistakes=$mistakes, plChars=$pl, correctChars=$chars, "
        . "minutes=$minutes, seconds=$seconds");
}

?>
