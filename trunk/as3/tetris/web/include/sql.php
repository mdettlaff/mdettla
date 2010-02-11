<?php

function sql_escape($string) {
    return pg_escape_string($string);
}

?>
