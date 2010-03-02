<?php

include 'include/log.php';

$username = 'admin';
$password = 'tschipp';
if ($_SERVER['PHP_AUTH_USER'] != $username
        || $_SERVER['PHP_AUTH_PW'] != $password) {
    header('WWW-Authenticate: Basic realm="Log realm"');
    header('HTTP/1.0 401 Unauthorized');
    echo 'Nie masz uprawnień do oglądania tej strony.';
    if (!empty($_SERVER['PHP_AUTH_USER']) || !empty($_SERVER['PHP_AUTH_PW'])) {
        log_write("unsuccessful log access attempt with login \""
            . $_SERVER['PHP_AUTH_USER'] . "\" and password \""
            . $_SERVER['PHP_AUTH_PW'] . "\"");
    }
    exit;
}

?>
<html>
  <head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8">
  </head>
  <body>
    <h2>Log</h2>

    <table cellspacing="8">
      <tr>
        <th>nr</th>
        <th>Data</th>
        <th>IP</th>
        <th>Wiadomość</th>
      </tr>
<?php

$query = "
    SELECT id_log, date_added, ip, message
        FROM tt.log
        ORDER BY id_log DESC
";

$result = pg_query($query);
if ($result) {
    while($row = pg_fetch_assoc($result)) {
        echo "      <tr>\n";
        echo '        <td>' . htmlspecialchars($row['id_log']) . "</td>\n";
        echo '        <td>' . str_replace(' ', '&nbsp;',
            htmlspecialchars($row['date_added'])) . "</td>\n";
        echo '        <td>' . htmlspecialchars($row['ip']) . "</td>\n";
        echo '        <td>' . htmlspecialchars($row['message']) . "</td>\n";
        echo "      </tr>\n";
    }
} else {
    echo("<tr><td colspan=\"4\">ERROR: problem with query</td></tr>\n");
    log_write("ERROR: problem with query: $query (" . pg_last_error() . ')');
}

?>
    </table>
  </body>
</html>
