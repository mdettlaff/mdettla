<?php

include 'include/log.php';

session_start();
log_write('user ' . $_SESSION['username'] . ' logged out');
session_unset();
session_destroy();

?>

<?php include 'template/top.php'; ?>

Zostałeś wylogowany pomyślnie.

<?php include 'template/bottom.php'; ?>
