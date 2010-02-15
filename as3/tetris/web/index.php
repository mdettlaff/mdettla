<?php session_start(); ?>

<?php include 'template/top.php'; ?>

<h2>Tetris</h2>

<?php
if (isset($_SESSION['username'])) {
    echo 'Witaj, ' . $_SESSION['username'] . "!<br />\n";
}
?>
Na tej stronie możesz zagrać w grę Tetris.

<?php include 'template/bottom.php'; ?>
