<?php

include '../include/db_connection.php';

$query = "
    SELECT text
        FROM tt.texts
        ORDER BY RANDOM()
        LIMIT 1
";
$result = pg_query($query);
if (!$result) {
    exit('ERROR: Problem with query.');
}
$row = pg_fetch_assoc($result);
echo $row['text'];

?>
