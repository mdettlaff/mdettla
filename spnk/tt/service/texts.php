<?php

include '../include/log.php';
include '../include/utils.php';

function get_texts_count() {
    $query = "
        SELECT COUNT(text) AS texts_count FROM texts
    ";
    $result = mysql_query($query);
    if ($result) {
        $row = mysql_fetch_assoc($result);
        return $row['texts_count'];
    } else {
        log_write("ERROR: problem with query: $query ("
            . mysql_error() . ')');
        exit(1);
    }
}

header('Content-Type: text/xml');

session_start();

if (empty($_SESSION['random_bag_of_text_ids'])) {
    $shuffled_numbers = range(1, get_texts_count());
    shuffle($shuffled_numbers);
    $_SESSION['random_bag_of_text_ids'] = $shuffled_numbers;
}
$text_offset = array_pop($_SESSION['random_bag_of_text_ids']) - 1;

$query = "
    SELECT text FROM texts LIMIT $text_offset, 1
";
$result = mysql_query($query);
if ($result) {
    $_SESSION['ttlog_h_data'] = rand_str(32);
    $row = mysql_fetch_assoc($result);
    echo "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
    echo "<response>\n";
    echo '<text>' . $row['text'] . "</text>\n";
    echo '<hData>' . $_SESSION['ttlog_h_data'] . "</hData>\n";
    echo '</response>';
} else {
    log_write("ERROR: problem with query: $query (" . mysql_error() . ')');
    exit(1);
}

?>
