<?php

function log_write($msg) { 
    $file = fopen('../log/log.txt', 'a');
    $entry = '['.date('Y/m/d h:i:s', mktime()).'] '.$msg;
    fwrite($file, $entry."\n");
    fclose($file);
}

?>
