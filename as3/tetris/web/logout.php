<?php

session_start();
session_unset();
session_destroy();

?>

<?php include 'template/top.php' ?>

Zostałeś wylogowany pomyślnie.

<?php include 'template/bottom.php' ?>
