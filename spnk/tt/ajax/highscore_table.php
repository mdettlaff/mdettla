<?php

include '../include/log.php';
include '../include/utils.php';

header('Content-Type: text/xml; charset=utf-8');

$MAX_HIGHSCORE_SIZE = 200;

$from_place = $_GET['from_place'];
if (!is_numeric($from_place)) {
    $from_place = 1;
}
$to_place = $_GET['to_place'];
if (!is_numeric($to_place)) {
    $to_place = $MAX_HIGHSCORE_SIZE;
}
if ($from_place > $to_place) {
    $to_place = $from_place;
}

echo "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
echo "<highscore>\n";
$result = mysql_query("
    SELECT COUNT(id_highscore) AS total_size FROM highscore
");
if ($result) {
    $row = mysql_fetch_assoc($result);
    $total_highscore_size = $row['total_size'];
    if ($total_highscore_size > $MAX_HIGHSCORE_SIZE) {
        $total_highscore_size = $MAX_HIGHSCORE_SIZE;
    }
    echo "<totalSize>" . $total_highscore_size . "</totalSize>\n";
} else {
    log_write("ERROR: problem with query: $query (" . mysql_error() . ')');
}
$result = mysql_query("
    SELECT username, speed, corrections, chars, minutes, seconds
        FROM highscore
        ORDER BY speed DESC
        LIMIT " . ($from_place - 1) . ", " . ($to_place - $from_place + 1) . "
");
if ($result) {
    $i = 0;
    while ($row = mysql_fetch_assoc($result)) {
        $correctness = sprintf('%.1f',
            ($row['chars'] - $row['corrections']) / $row['chars'] * 100);
        echo "<entry>\n";
        echo "<rank>" . ($from_place + $i) . "</rank>\n";
        $username = htmlspecialchars(latin2_to_utf8($row['username']));
        echo "<username><![CDATA[" . $username . "]]></username>\n";
        echo "<speed>" . $row['speed'] . "</speed>\n";
        echo "<correctness>" . $correctness . "</correctness>\n";
        echo "<chars>" . $row['chars'] . "</chars>\n";
        echo "<minutes>" . $row['minutes'] . "</minutes>\n";
        echo "<seconds>" . $row['seconds'] . "</seconds>\n";
        echo "</entry>\n";
        $i++;
    }
} else {
    log_write("ERROR: problem with query: $query (" . mysql_error() . ')');
}
echo "</highscore>\n";

?>
