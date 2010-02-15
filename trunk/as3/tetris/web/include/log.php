<?php

function log_write($msg) {
    $file = fopen('log/log.txt', 'a');
    if (!$file) {
        $file = fopen('../log/log.txt', 'a');
    }
    if ($file) {
        $entry = date('Y-m-d H:i:s') . " $msg";
        fwrite($file, "$entry\n");
        fclose($file);
    } else {
        echo 'ERROR: log file not found!';
    }
}

?>
