<?php

function log_write($msg) {
    if (isset($_SERVER['HTTP_X_FORWARDED_FOR'])) {
        $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
    } else {
        $ip = $_SERVER['REMOTE_ADDR'];
    }
    $ip = mysql_real_escape_string($ip);
    $msg = mysql_real_escape_string($msg);
    $query = "
        INSERT INTO log
            (date_added, ip, message)
            VALUES
            (NOW(), '$ip', '$msg')
    ";
    mysql_query($query) or die('ERROR: cannot write to log!');
}

?>
