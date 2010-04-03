<?php

session_start();

if (isset($_POST['username'])) {
    $MONTH = 3600 * 24 * 30;
    setcookie('username', trim($_POST['username']), time() + 3 * $MONTH);
}

include '../include/log.php';
include '../include/utils.php';

function validate_username($username) {
    return !empty($username)
        && strlen($username) <= 32 && strlen($username) >= 3;
}

function validate($username, $speed, $mistakes, $corrections,
        $pl, $chars, $minutes, $seconds, $time_verifier) {
    return validate_username($username)
        && is_numeric(str_replace(',', '.', $speed))
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

function query_required_speed($min_required_speed, $max_highscore_size) {
    $query = "
        SELECT COUNT(id_highscore) AS current_size FROM highscore
    ";
    $result = pg_query($query) or log_write(
        "ERROR: problem with query: $query (" . pg_last_error() . ')');
    $row = pg_fetch_assoc($result);
    $current_size = $row['current_size'];
    if ($current_size > $max_highscore_size) {
        $current_size = $max_highscore_size;
    }
    if ($current_size > 0) {
        $query = "
            SELECT
                speed AS required_speed
                FROM highscore
                ORDER BY speed DESC
                LIMIT 1 OFFSET " . ($current_size - 1) . "
        ";
        $result = pg_query($query) or log_write(
            "ERROR: problem with query: $query (" . pg_last_error() . ')');
        $row = pg_fetch_assoc($result);
        if ($row && $current_size == $max_highscore_size) {
            $required_speed = $row['required_speed'];
        }
    }
    if (empty($required_speed)) {
        $required_speed = $min_required_speed;
    }
    if (isset($_COOKIE['username'])) {
        $username = pg_escape_string($_COOKIE['username']);
        $query = "
            SELECT
                speed AS speed_for_user
                FROM highscore
                WHERE username = '$username'
                LIMIT 1
        ";
        $result = pg_query($query) or log_write(
            "ERROR: problem with query: $query (" . pg_last_error() . ')');
        $row = pg_fetch_assoc($result);
        if ($row && $row['speed_for_user'] > $required_speed) {
            $required_speed = $row['speed_for_user'];
        }
    }
    return $required_speed;
}

$H_KEY = 'secret2';
$MAX_HIGHSCORE_SIZE = 150;
$MIN_REQUIRED_SPEED = 120;

$required_speed = query_required_speed(
        $MIN_REQUIRED_SPEED, $MAX_HIGHSCORE_SIZE);
if ($_GET['q'] == 'get_threshold') {
    $_SESSION['hs_h_data'] = rand_str(32);
    header('Content-Type: text/xml');
    echo "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
    echo "<response>\n";
    echo '<requiredSpeed>' . $required_speed . "</requiredSpeed>\n";
    if (isset($_COOKIE['username'])) {
        echo "<username><![CDATA[" . $_COOKIE['username'] . "]]></username>\n";
    } else {
        echo "<username />\n";
    }
    echo '<hData>' . $_SESSION['hs_h_data'] . "</hData>\n";
    echo '</response>';
} else {
    $username = trim($_POST['username']);
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
        $username = pg_escape_string($username);
        $speed = pg_escape_string($speed);
        $mistakes = pg_escape_string($mistakes);
        $corrections = pg_escape_string($corrections);
        $pl = pg_escape_string($pl);
        $chars = pg_escape_string($chars);
        $minutes = pg_escape_string($minutes);
        $seconds = pg_escape_string($seconds);
        $query = "
            SELECT
                speed AS speed_for_user
                FROM highscore
                WHERE username = '$username'
                LIMIT 1
        ";
        $result = pg_query($query) or log_write(
            "ERROR: problem with query: $query (" . pg_last_error() . ')');
        $row = pg_fetch_assoc($result);
        if (!$row) {
            $query = "
                INSERT INTO highscore
                    (date_added, ip, username, speed, mistakes,
                        corrections, pl, chars, minutes, seconds)
                    VALUES
                    (NOW(), '$ip', '$username', $speed, $mistakes,
                        $corrections, '$pl', $chars, $minutes, $seconds)
            ";
            pg_query($query) or log_write("ERROR: problem with query: $query ("
                . pg_last_error() . ')');
        } else if ($row && $speed > $row['speed_for_user']) {
            $query = "
                UPDATE highscore
                    SET date_added = NOW(),
                        ip = '$ip',
                        speed = $speed,
                        mistakes = $mistakes,
                        corrections = $corrections,
                        pl = '$pl',
                        chars = $chars,
                        minutes = $minutes,
                        seconds = $seconds
                    WHERE username = '$username'
            ";
            pg_query($query) or log_write(
                "ERROR: problem with query: $query (" . pg_last_error() . ')');
        }
    }

    unset($_SESSION['hs_h_data']);
    $_SESSION['last_hs_submit_time'] = time();
}

?>
