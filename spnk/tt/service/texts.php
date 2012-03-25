<?php

session_start();
setcookie('PHPSESSID', session_id(), 0, '/', '.hosting8686472.az.pl');

mysql_connect('62.146.68.172', 'a06062ak_spnk', 'secretdbpass');
mysql_select_db('a06062ak_spnk');

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

header('Content-Type: text/xml; charset=utf-8');

if (empty($_SESSION['shuffle_bag'])) {
    $shuffled_numbers = range(1, get_texts_count());
    shuffle($shuffled_numbers);
    $_SESSION['shuffle_bag'] = $shuffled_numbers;
}
$text_offset = array_pop($_SESSION['shuffle_bag']) - 1;

$query = "
    SELECT text FROM texts LIMIT $text_offset, 1
";
$result = mysql_query($query);
if ($result) {
    $_SESSION['ttlog_h_data'] = rand_str(32);
    $row = mysql_fetch_assoc($result);
    echo "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
    echo "<response>\n";
    echo '<text>' . latin2_to_utf8($row['text']) . "</text>\n";
    echo '<hData>' . $_SESSION['ttlog_h_data'] . "</hData>\n";
    echo '</response>';
} else {
    log_write("ERROR: problem with query: $query (" . mysql_error() . ')');
    exit(1);
}

?>
