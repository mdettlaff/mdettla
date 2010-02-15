<?php

session_start();

include '../include/db_connection.php';
include '../include/sql.php';
include '../include/log.php';

$score = $_GET['score'];
$id_user = $_SESSION['id_user'];
$username = $_SESSION['username'];
if (!isset($id_user) || !isset($username)) {
    $id_user = 1;
    $username = 'guest';
}

if (!isset($score)) {
    log_write('failed attempt at adding score');
    exit('ERROR: Missing parameter.');
}

$query = "
    INSERT INTO tetris.highscore
        (score, id_user)
        VALUES
        (" . sql_escape($score) . ", " . sql_escape($id_user) . ");
";

$result = pg_query($query);
if (!$result) {
    echo 'ERROR: Problem with query.';
    log_write("ERROR: Problem with query $query\n" . pg_last_error());
} else {
    log_write("score $score added for user $username");
    echo 'OK';
}

?>
