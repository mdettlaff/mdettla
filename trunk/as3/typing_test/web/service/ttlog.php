<?php

include '../include/log.php';

session_start();

function validate($speed, $mistakes, $pl, $chars, $minutes, $seconds) {
    return is_numeric(str_replace(',', '.', $speed)) && is_numeric($mistakes)
        && ($pl == 'true' || $pl == 'false') && is_numeric($chars)
        && is_numeric($minutes) && is_numeric($seconds);
}

function isSubmittedTooSoon($submit_time, $last_submit_time) {
    $min_time_between_submits = 30;
    return (is_numeric($last_submit_time)
        && ($submit_time - $last_submit_time < $min_time_between_submits));
}

$speed = $_POST['speed'];
$mistakes = $_POST['mistakes'];
$pl = $_POST['plChars'];
$chars = $_POST['correctChars'];
$minutes = $_POST['minutes'];
$seconds = $_POST['seconds'];

$MAX_MISTAKES = 25;

if (validate($speed, $mistakes, $pl, $chars, $minutes, $seconds)) {
    $current_time = time();
    if (isSubmittedTooSoon($current_time, $_SESSION['last_submit_time'])) {
        log_write('entry not added to ttlog, submitted too soon; '
            . 'time=' . $current_time . ', last_submit_time='
            . $_SESSION['last_submit_time'] . '; '
            . 'POST parameters: ' . print_r($_POST, true));
    } else if ($mistakes <= $MAX_MISTAKES) {
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
        pg_query($query) or log_write("ERROR: problem with query: $query ("
            . pg_last_error() . ')');
    }
} else {
    echo 'Does not compute.';
    log_write('entry not added to ttlog, validation failed; POST parameters: '
        . print_r($_POST, true));
}

$_SESSION['last_submit_time'] = time();

?>
