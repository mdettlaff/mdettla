<?php

session_start();
setcookie('PHPSESSID', session_id(), 0, '/', '.szybkiepisanie.webpark.pl');

include '../include/log.php';
include '../include/utils.php';

function validate($username, $speed, $mistakes, $corrections,
        $pl, $chars, $minutes, $seconds, $time_verifier) {
    return !empty($username) && strlen($username) <= 32
        && strlen($username) >= 3 && is_numeric(str_replace(',', '.', $speed))
        && is_numeric($mistakes) && is_numeric($corrections)
        && ($pl == 'true' || $pl == 'false') && is_numeric($chars)
        && is_numeric($minutes) && is_numeric($seconds)
        && is_numeric($time_verifier);
}

function is_submitted_too_soon($submit_time, $last_submit_time) {
    $min_time_between_submits = 30;
    return (is_numeric($last_submit_time)
        && ($submit_time - $last_submit_time < $min_time_between_submits));
}

$H_KEY = 'secret2';
$MAX_HIGHSCORE_SIZE = 200;
$MIN_REQUIRED_SPEED = 120;

mysql_connect();

$result = mysql_query("
    SELECT COUNT(id_highscore) AS current_size FROM highscore
") or log_write("ERROR: problem with query: $query (" . mysql_error() . ')');
$row = mysql_fetch_assoc($result);
$current_size = $row['current_size'];
if ($current_size > $MAX_HIGHSCORE_SIZE) {
    $current_size = $MAX_HIGHSCORE_SIZE;
}
$result = mysql_query("
    SELECT
        speed AS required_speed
        FROM highscore
        ORDER BY speed DESC
        LIMIT " . ($current_size - 1) . ", 1
") or log_write("ERROR: problem with query: $query (" . mysql_error() . ')');
$row = mysql_fetch_assoc($result);
if ($row && $current_size == $MAX_HIGHSCORE_SIZE) {
    $required_speed = $row['required_speed'];
}
if (empty($required_speed)) {
    $required_speed = $MIN_REQUIRED_SPEED;
}
if ($_GET['q'] == 'get_threshold') {
    $_SESSION['hs_h_data'] = rand_str(32);
    header('Content-Type: text/xml');
    echo "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
    echo "<response>\n";
    echo '<requiredSpeed>' . $required_speed . "</requiredSpeed>\n";
    echo '<hData>' . $_SESSION['hs_h_data'] . "</hData>\n";
    echo '</response>';
} else {
    $username = $_POST['username'];
    $speed = $_POST['speed'];
    $mistakes = $_POST['mistakes'];
    $corrections = $_POST['corrections'];
    $pl = $_POST['plChars'];
    $chars = $_POST['correctChars'];
    $minutes = $_POST['minutes'];
    $seconds = $_POST['seconds'];
    $h = $_POST['h'];
    $time_verifier = $_POST['timeVerifier'];

    $MAX_MISTAKES = 0;

    $current_time = time();
    if (!validate($username, $speed, $mistakes, $corrections,
            $pl, $chars, $minutes, $seconds, $time_verifier)) {
        echo 'Does not compute.';
        log_write('entry not added to highscore, validation failed; '
            . 'POST parameters: ' . print_r($_POST, true));
    } else if (is_submitted_too_soon(
            $current_time, $_SESSION['last_hs_submit_time'])) {
        log_write('entry not added to highscore, submitted too soon; '
            . 'time=' . $current_time . ', last_hs_submit_time='
            . $_SESSION['last_hs_submit_time'] . '; '
            . 'POST parameters: ' . print_r($_POST, true));
    } else if (!verify_typing_time($minutes, $seconds, $time_verifier)) {
        log_write('entry not added to highscore, suspicious typing time; '
            . 'POST parameters: ' . print_r($_POST, true));
    } else if (!is_hmac_valid($h, $_SESSION['hs_h_data'] . ':'
            . $speed . ':' . $mistakes . ':' . $corrections . ':' . $pl . ':'
            . $minutes . ':' . $seconds . ':' . $time_verifier, $H_KEY)) {
        log_write('entry not added to highscore, wrong HMAC; '
            . 'hs_h_data=' . $_SESSION['hs_h_data'] . '; '
            . 'POST parameters: ' . print_r($_POST, true));
    } else if ($speed < $required_speed || $mistakes > $MAX_MISTAKES
            || $pl == 'false' || ($chars - $corrections) / $chars * 100 < 95) {
        log_write('entry not added to highscore, test result not accepted; '
            . "speed=$speed, required_speed=$required_speed; "
            . 'POST parameters: ' . print_r($_POST, true));
    } else {
        // konwersja polskiego u³amka dziesiêtnego (przecinek)
        // na amerykañski (kropka)
        $speed = str_replace(',', '.', $speed);
        if (isset($_SERVER['HTTP_X_FORWARDED_FOR'])) {
            $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
        } else {
            $ip = $_SERVER['REMOTE_ADDR'];
        }
        $username = mysql_real_escape_string(utf8_to_latin2($username));
        $speed = mysql_real_escape_string($speed);
        $mistakes = mysql_real_escape_string($mistakes);
        $corrections = mysql_real_escape_string($corrections);
        $pl = mysql_real_escape_string($pl);
        $chars = mysql_real_escape_string($chars);
        $minutes = mysql_real_escape_string($minutes);
        $seconds = mysql_real_escape_string($seconds);
        $query = "
            INSERT INTO highscore
                (date_added, ip, username, speed, mistakes, corrections,
                    pl, chars, minutes, seconds)
                VALUES
                (NOW(), '$ip', '$username', $speed, $mistakes, $corrections,
                    '$pl', $chars, $minutes, $seconds)
        ";
        mysql_query($query) or log_write("ERROR: problem with query: $query ("
            . mysql_error() . ')');
    }

    unset($_SESSION['hs_h_data']);
    $_SESSION['last_hs_submit_time'] = time();
}

?>
