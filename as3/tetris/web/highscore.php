<?php session_start() ?>

<?php include 'template/top.php' ?>

<?php

include 'include/db_connection.php';

function translate_username($username) {
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

$query = '
SELECT username, score
    FROM tetris.highscore h
        LEFT JOIN tetris.users u
        ON (h.id_user = u.id_user)
    WHERE score > 0
    ORDER BY score DESC
';

$result = pg_query($query);
if (!$result) {
    echo 'ERROR: Problem with query '.$query.'<br/>';
    echo pg_last_error();
}
while($row = pg_fetch_assoc($result)) {
?>
    <tr>
        <td><?php echo translate_username($row['username']) ?></td>
        <td><?php echo htmlspecialchars($row['score']) ?></td>
    </tr>
<?php
}
?>

</table>

<?php include 'template/bottom.php' ?>
