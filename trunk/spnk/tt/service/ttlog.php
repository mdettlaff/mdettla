<?php

session_start();
setcookie('PHPSESSID', session_id(), 0, '/', '.hosting8686472.az.pl');

mysql_connect('62.146.68.172', 'a06062ak_spnk', 'secretdbpass');
mysql_select_db('a06062ak_spnk');

include '../include/log.php';
include '../include/utils.php';

function validate($speed, $mistakes, $corrections, $pl, $chars,
        $minutes, $seconds, $time_verifier) {
    return is_numeric(str_replace(',', '.', $speed)) && is_numeric($mistakes)
        && is_numeric($corrections) && ($pl == 'true' || $pl == 'false')
        && is_numeric($chars) && is_numeric($minutes) && is_numeric($seconds)
        && is_numeric($time_verifier);
}

function is_submitted_too_soon($submit_time, $last_submit_time) {
    $min_time_between_submits = 30;
    return (is_numeric($last_submit_time)
        && ($submit_time - $last_submit_time < $min_time_between_submits));
}

$speed = $_POST['speed'];
$mistakes = $_POST['mistakes'];
$corrections = $_POST['corrections'];
$pl = $_POST['plChars'];
$chars = $_POST['correctChars'];
$minutes = $_POST['minutes'];
$seconds = $_POST['seconds'];
$h = $_POST['h'];
$time_verifier = $_POST['timeVerifier'];

$H_KEY = 'secret1';
$MAX_MISTAKES = 25;

$current_time = time();
if (!validate($speed, $mistakes, $corrections, $pl, $chars,
        $minutes, $seconds, $time_verifier)) {
    echo 'Does not compute.';
    log_write('entry not added to ttlog, validation failed; '
        . 'POST parameters: ' . print_r($_POST, true));
} else if (is_submitted_too_soon(
        $current_time, $_SESSION['last_submit_time'])) {
    log_write('entry not added to ttlog, submitted too soon; '
        . 'time=' . $current_time . ', last_submit_time='
        . $_SESSION['last_submit_time'] . '; '
        . 'POST parameters: ' . print_r($_POST, true));
} else if (!verify_typing_time($minutes, $seconds, $time_verifier)) {
    log_write('entry not added to ttlog, suspicious typing time; '
        . 'POST parameters: ' . print_r($_POST, true));
/* wy³±czamy sprawdzanie HMAC, bo ttlog_h_data nie zawsze siê zapisuje
} else if (!is_hmac_valid($h, $_SESSION['ttlog_h_data'] . ':'
        . $speed . ':' . $mistakes . ':' . $corrections . ':' . $pl . ':'
        . $minutes . ':' . $seconds . ':' . $time_verifier, $H_KEY)) {
    log_write('entry not added to ttlog, wrong HMAC; '
        . 'ttlog_h_data=' . $_SESSION['ttlog_h_data'] . '; '
        . 'POST parameters: ' . print_r($_POST, true) . '; '
        . 'user agent: ' . $_SERVER['HTTP_USER_AGENT']);
*/
} else if ($mistakes <= $MAX_MISTAKES) {
    // konwersja polskiego u³amka dziesiêtnego (przecinek)
    // na amerykañski (kropka)
    $speed = str_replace(',', '.', $speed);
    if (isset($_SERVER['HTTP_X_FORWARDED_FOR'])) {
        $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
    } else {
        $ip = $_SERVER['REMOTE_ADDR'];
    }
    $speed = mysql_real_escape_string($speed);
    $mistakes = mysql_real_escape_string($mistakes);
    $corrections = mysql_real_escape_string($corrections);
    $pl = mysql_real_escape_string($pl);
    $chars = mysql_real_escape_string($chars);
    $minutes = mysql_real_escape_string($minutes);
    $seconds = mysql_real_escape_string($seconds);
    if (isset($_COOKIE['username'])) {
        $username = '\'' . mysql_real_escape_string(
            utf8_to_latin2($_COOKIE['username'])) . '\'';
    } else {
        $username = 'NULL';
    }
    $query = "
        INSERT INTO ttlog
            (date_added, ip, speed, mistakes, corrections,
                pl, chars, minutes, seconds, username)
            VALUES
            (NOW(), '$ip', $speed, $mistakes, $corrections,
                '$pl', $chars, $minutes, $seconds, $username)
    ";
    mysql_query($query) or log_write("ERROR: problem with query: $query ("
        . mysql_error() . ')');
}

unset($_SESSION['ttlog_h_data']);
$_SESSION['last_submit_time'] = time();

?>
