<?php

include 'db_connection.php';

function log_write($msg) {
    if (isset($_SERVER['HTTP_X_FORWARDED_FOR'])) {
        $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
    } else {
        $ip = $_SERVER['REMOTE_ADDR'];
    }
    $ip = pg_escape_string($ip);
    $msg = pg_escape_string($msg);
    $query = "
        INSERT INTO tt.log
            (date_added, ip, message)
            VALUES
            (NOW(), '$ip', \$\$$msg\$\$)
    ";
    pg_query($query) or die('ERROR: cannot write to log!');
}

?>
