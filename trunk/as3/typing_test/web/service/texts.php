<?php

include '../include/db_connection.php';

function get_texts_count() {
    $query = "
        SELECT COUNT(text) AS texts_count FROM tt.texts
    ";
    $result = pg_query($query) or die('ERROR: Problem with query.');
    $row = pg_fetch_assoc($result);
    return $row['texts_count'];
}

session_start();

if (empty($_SESSION['random_bag_of_text_ids'])) {
    $shuffled_numbers = range(1, get_texts_count());
    shuffle($shuffled_numbers);
    $_SESSION['random_bag_of_text_ids'] = $shuffled_numbers;
}
$text_offset = array_pop($_SESSION['random_bag_of_text_ids']) - 1;

$query = "
    SELECT text FROM tt.texts OFFSET $text_offset LIMIT 1
";
$result = pg_query($query) or die('ERROR: Problem with query.');
$row = pg_fetch_assoc($result);
echo $row['text'];

?>
