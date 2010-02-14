<?php session_start() ?>

<?php include 'template/top.php' ?>

<?php

include 'include/db_connection.php';
include 'include/sql.php';
include 'include/log.php';

$form_username = $_POST['username'];
$form_password = $_POST['passwd'];
if (!empty($form_username) && !empty($form_password)) {
    $query = '
        SELECT id_user FROM tetris.users
            WHERE username = \''.sql_escape($form_username).'\'
                AND password = \''.sql_escape($form_password).'\'
            LIMIT 1;
    ';
    $result = pg_query($query);
    if ($result) {
        $row = pg_fetch_assoc($result);
        if (!empty($row['id_user'])) {
            $_SESSION['id_user'] = $row['id_user'];
            $_SESSION['username'] = $form_username;
            echo 'Zalogowany jako '.$_SESSION['username'];
            log_write($form_username.' logged in');
        } else {
            log_write('failed login as '.$form_username.' with password '.
                $form_password);
            echo 'Nieprawidłowa nazwa użytkownika lub hasło.';
        }
    } else {
        log_write('ERROR: Problem with query '.$query);
        log_write(pg_last_error());
        echo 'Błąd bazy danych.';
    }
} else if (empty($form_username) && empty($form_password)) {
    echo 'Musisz podać nazwę użytkownika i hasło.';
} else if (empty($form_username)) {
    echo 'Musisz podać nazwę użytkownika.';
} else if (empty($form_password)) {
    echo 'Musisz podać hasło.';
}

?>

<?php include 'template/bottom.php' ?>
