<?php

include 'db_connection.php';

function log_write($msg) {
    $msg ;
    $query = "
        INSERT INTO tt.log
            (date_added, message)
            VALUES
            (NOW(), \$\$" . pg_escape_string($msg) . "\$\$)
    ";
    pg_query($query) or die('ERROR: cannot write to log!');
}

?>
