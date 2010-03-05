<?php

include '../include/log.php';

header('Content-Type: text/xml');

$HIGHSCORE_SIZE = 25;
$CHARS_ALLOWED_IN_USERNAME = "A-Za-z0-9 _.'-";

echo "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n";
$result = pg_query("
    SELECT username, speed, corrections, chars, minutes, seconds
        FROM tt.highscore
        ORDER BY speed DESC
        LIMIT $HIGHSCORE_SIZE
");
echo "<highscore>\n";
if ($result) {
    $i = 1;
    while ($row = pg_fetch_assoc($result)) {
        $correctness = sprintf('%.1f',
            ($row['chars'] - $row['corrections']) / $row['chars'] * 100);
        echo "<entry>\n";
        echo "<rank>$i</rank>\n";
        $username = ereg_replace('[^'. $CHARS_ALLOWED_IN_USERNAME . ']', '',
            $row['username']);
        echo "<username>" . $username . "</username>\n";
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
