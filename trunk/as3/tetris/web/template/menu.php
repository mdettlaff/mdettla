        <h3>Menu</h3>
        <ul>
          <li><a href="index.php">Strona główna</a></li>
          <li><a href="tetris.php">Zagraj online</a></li>
          <li><a href="highscore.php">Najlepsze wyniki</a></li>
<?php
if (!isset($_SESSION['username'])) {
    echo "          <li><a href=\"login.php\">Zaloguj się</a></li>\n";
} else {
    echo "          <li><a href=\"logout.php\">Wyloguj się</a></li>\n";
}
?>
        </ul>
