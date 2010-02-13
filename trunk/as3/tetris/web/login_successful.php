<?php session_start() ?>

<?php include 'template/top.php' ?>

<?php
echo 'Zalogowany jako '.$_SESSION['username'];
?>

<?php include 'template/bottom.php' ?>
