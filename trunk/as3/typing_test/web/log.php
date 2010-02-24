<html>
  <head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8">
  </head>
  <body>
    <h2>Log</h2>

    <table cellspacing="8">
      <tr>
        <th>LP</th>
        <th>Data</th>
        <th>Wiadomość</th>
      </tr>
<?php

include 'include/db_connection.php';

$query = "
    SELECT id_log, date_added, message
        FROM tt.log
        ORDER BY id_log DESC
";

$result = pg_query($query);
if (!$result) {
    log_write("ERROR: Problem with query $query");
    log_write(pg_last_error());
}
while($row = pg_fetch_assoc($result)) {
    echo "      <tr>\n";
    echo '        <td>' . htmlspecialchars($row['id_log']) . "</td>\n";
    echo '        <td>' . str_replace(' ', '&nbsp;',
        htmlspecialchars($row['date_added'])) . "</td>\n";
    echo '        <td>' . htmlspecialchars($row['message']) . "</td>\n";
    echo "      </tr>\n";
}

?>
    </table>
  </body>
</html>
