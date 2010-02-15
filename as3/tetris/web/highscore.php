<?php session_start(); ?>

<?php include 'template/top.php'; ?>

<?php

include 'include/db_connection.php';

function process_username($username) {
    return $username != 'guest' ? htmlspecialchars($username) : 'Gość';
}

?>

<h2>Najlepsze wyniki</h2>

<table cellspacing="8">
  <tr>
    <th>Użytkownik</th>
    <th>Liczba punktów</th>
  </tr>
<?php
$query = "
    SELECT username, score
        FROM tetris.highscore
        LEFT JOIN tetris.users USING (id_user)
        WHERE score > 0
        ORDER BY score DESC;
";

$result = pg_query($query);
if (!$result) {
    log_write("ERROR: Problem with query $query");
    log_write(pg_last_error());
}
while($row = pg_fetch_assoc($result)) {
    echo "  <tr>\n";
    if ($row['username'] != $_SESSION['username']) {
        echo '    <td>' . process_username($row['username']) . "</td>\n";
        echo '    <td>' . htmlspecialchars($row['score']) . "</td>\n";
    } else {
        echo '    <td><b>' . process_username($row['username']) . "</b></td>\n";
        echo '    <td><b>' . htmlspecialchars($row['score']) . "</b></td>\n";
    }
    echo "  </tr>\n";
}
?>
</table>

<?php include 'template/bottom.php'; ?>
