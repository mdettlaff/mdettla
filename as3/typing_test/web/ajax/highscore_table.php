<?php

include '../include/log.php';

function escape_username($username) {
    $escaped = '';
    for ($i = 0; $i < strlen($username); $i++) {
        $c = substr($username, $i, 1);
        $charcode = ord($c);
        if ($charcode >= 32) {
            $escaped .= $c;
        }
    }
    $escaped = htmlspecialchars($escaped);
    return $escaped;
}

header('Content-Type: text/xml');

$MAX_HIGHSCORE_SIZE = 150;

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
$result = pg_query("
    SELECT COUNT(id_highscore) AS total_size FROM highscore
");
if ($result) {
    $row = pg_fetch_assoc($result);
    $total_highscore_size = $row['total_size'];
    if ($total_highscore_size > $MAX_HIGHSCORE_SIZE) {
        $total_highscore_size = $MAX_HIGHSCORE_SIZE;
    }
    echo "<totalSize>" . $total_highscore_size . "</totalSize>\n";
} else {
    log_write("ERROR: problem with query: $query (" . pg_last_error() . ')');
}
$result = pg_query("
    SELECT username, speed, corrections, chars, minutes, seconds
        FROM highscore
        ORDER BY speed DESC
        LIMIT " . ($to_place - $from_place + 1) . "
        OFFSET " . ($from_place - 1) . "
");
if ($result) {
    $i = 0;
    while ($row = pg_fetch_assoc($result)) {
        $correctness = sprintf('%.1f',
            ($row['chars'] - $row['corrections']) / $row['chars'] * 100);
        echo "<entry>\n";
        echo "<rank>" . ($from_place + $i) . "</rank>\n";
        $username = escape_username($row['username']);
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
    log_write("ERROR: problem with query: $query (" . pg_last_error() . ')');
}
echo "</highscore>\n";

?>
