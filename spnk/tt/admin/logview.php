<?php

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

mysql_connect('62.146.68.172', 'a06062ak_spnk', 'secretdbpass');
mysql_select_db('a06062ak_spnk');

include '../include/log.php';
include '../include/utils.php';

?>
<html>
  <head>
    <meta http-equiv="Content-type" content="text/html; charset=iso-8859-2">
    <style>
      .logview-table td {
        vertical-align: top;
      }
    </style>
  </head>
  <body>
    <h2>Log</h2>

    <table class="logview-table" cellspacing="8">
      <tr>
        <th>nr</th>
        <th>Data</th>
        <th>IP</th>
        <th>Wiadomość</th>
      </tr>
<?php

$query = "
    SELECT date_added, ip, message
        FROM log
        ORDER BY date_added DESC
";

$result = mysql_query($query);
$i = mysql_num_rows($result);
if ($result) {
    while ($row = mysql_fetch_assoc($result)) {
        $message = htmlspecialchars(utf8_to_latin2($row['message']));
        echo "      <tr>\n";
        echo '        <td>' . $i . "</td>\n";
        echo '        <td>' . str_replace(' ', '&nbsp;',
            htmlspecialchars($row['date_added'])) . "</td>\n";
        echo '        <td>' . htmlspecialchars($row['ip']) . "</td>\n";
        echo '        <td>' . $message . "</td>\n";
        echo "      </tr>\n";
        $i--;
    }
} else {
    echo("<tr><td colspan=\"4\">ERROR: problem with query</td></tr>\n");
    log_write("ERROR: problem with query: $query (" . mysql_error() . ')');
}

?>
    </table>
  </body>
</html>
