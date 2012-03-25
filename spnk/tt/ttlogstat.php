<?php

mysql_connect('62.146.68.172', 'a06062ak_spnk', 'secretdbpass');
mysql_select_db('a06062ak_spnk');

include 'include/log.php';

function compute_correctness($chars, $mistakes, $corrections) {
    if (isset($corrections)) {
        return sprintf('%.1f',
            ($chars - $corrections) / ($chars + $mistakes) * 100);
    }
    return '';
}

function td_cut($content, $maxlen) {
    $result = '';
    if (strlen($content) > $maxlen) {
        $result .= '<td title="' . htmlspecialchars($content) . '">';
        $result .= htmlspecialchars(
            trim(substr($content, 0, $maxlen - 1))) . '...';
    } else {
        $result .= '<td>';
        $result .= htmlspecialchars($content);
    }
    $result .= '</td>';
    return $result;
}

$username = 'admin';
$password = 'tschipp';
if ($_SERVER['PHP_AUTH_USER'] != $username
        || $_SERVER['PHP_AUTH_PW'] != $password) {
    header('WWW-Authenticate: Basic realm="Log realm"');
    header('HTTP/1.0 401 Unauthorized');
    echo 'Nie masz uprawnieñ do ogl±dania tej strony.';
    if (!empty($_SERVER['PHP_AUTH_USER']) || !empty($_SERVER['PHP_AUTH_PW'])) {
        log_write("unsuccessful ttlog access attempt with login \""
            . $_SERVER['PHP_AUTH_USER'] . "\" and password \""
            . $_SERVER['PHP_AUTH_PW'] . "\"");
    }
    exit;
}

?>
<html>
<head>
<meta http-equiv="Content-type" content="text/html; charset=iso-8859-2">
<style>
  .vertical-bar {
    width: 25px;
    background-color: #2C7EFC;
    position: relative;
    bottom: 0;
  }
  .large-table th {
    text-align: center;
  }
  .large-table td {
    padding: 3 4 2 2;
    text-align: right;
  }
  .large-table td:nth-child(3) {
    text-align: left;
    padding-left: 7;
  }
  .large-table td:nth-child(10) {
    text-align: left;
  }
  .large-table tr:nth-child(1) {
    background: #D0D0D0
  }
  .large-table tr:nth-child(2n+2) {
    background: #FFFFFF
  }
  .large-table tr:nth-child(2n+3) {
    background: #E0E0E0
  }
</style>
</head>
<body>

<?php

// wykres s³upkowy z prêdko¶ciami
$maxspeed = 400;
for ($i = 0; $i < $maxspeed / 50; $i++) {
    $xv = $i * 50;
    $r = mysql_query("
        SELECT COUNT(speed) AS scount
            FROM ttlog
            WHERE speed > " . $xv . " AND speed < " . ($xv + 50)
    );
    $row = mysql_fetch_array($r);
    $data[$i] = $row['scount'];
}
$r = mysql_query("
    SELECT COUNT(speed) AS scount FROM ttlog
        WHERE speed > " . ($i * 50));
$row = mysql_fetch_array($r);
$data[$i] = $row['scount'];

$sum = array_sum($data);
$height = 320;
$y = 0;
echo "<table border=\"0\">\n<tr>\n";
for ($i = 0; $i < $maxspeed / 50 + 1; $i++) {
    echo "<td align=\"center\" valign=\"bottom\" width=\"40\">";
    $y = ($data[$i] / $sum) * $height;
    $perc = ($data[$i] / $sum) * 100;
    printf("%.0f%%", $perc);
    $yint = (int)$y;
    if ($yint > 0) {
        echo "<br><div class=\"vertical-bar\" style=\"height: $yint;\"></div>";
    }
    echo "</td>\n";
}
echo "</tr>\n<tr>\n";
for ($i = 0; $i < $maxspeed / 50 + 1; $i++) {
    $xvalue = $i * 50;
    echo '<td><small>' . $xvalue . '-';
    if ($xvalue < $maxspeed) {
        echo $xvalue + 50;
    } else {
        echo '...';
    }
    echo "</small></td>\n";
}
echo "</tr>\n</table>\n";
echo "<br>\n\n";

// statystyka pisania z polskimi znakami
$r = mysql_query("
    SELECT COUNT(pl) AS pl_on FROM ttlog WHERE pl = 'true'
");
$row = mysql_fetch_array($r);
$plCharsON = (double)$row['pl_on'];
$r = mysql_query("
    SELECT COUNT(pl) AS pl_off FROM ttlog WHERE pl = 'false'
");
$row = mysql_fetch_array($r);
$plCharsOFF = (double)$row['pl_off'];
$plCharsSUM = $plCharsON + $plCharsOFF;
echo "<table cellspacing=\"0\" cellpadding=\"0\" border=\"0\">\n<tr>\n";
echo "<td colspan=\"4\" height=\"25\" valign=\"top\">";
echo "<b>Polskie znaki w³±czone:</b></td></tr>\n<tr>\n";
printf("<td>tak %.0f%%&nbsp;</td>"
    . "<td width=\"%.0f\" bgcolor=\"#00FF00\"></td>\n",
    $plCharsON / $plCharsSUM * 100, $plCharsON / $plCharsSUM * 275);
printf("<td width=%.0f bgcolor=\"#FF0000\"></td>"
    . "<td>&nbsp;%.0f%% nie</td>\n",
    $plCharsOFF / $plCharsSUM * 275, $plCharsOFF / $plCharsSUM * 100);
echo "</tr>\n</table>\n<br>\n\n";

// ¶rednia prêdko¶æ
// obliczaj±c ¶redni± uwzglêdniamy tylko prêdko¶ci wiêksze ni¿ 50
$r = mysql_query(
    'SELECT AVG(speed) AS average_speed FROM ttlog WHERE speed > 50'
);
$row = mysql_fetch_array($r);
$mean = $row['average_speed'];
printf("<big>¦rednia prêdko¶æ: <b>%.1f</b> znaków/min</big><br>\n", $mean);
echo "<hr width=\"400\" align=\"left\">\n\n";

$USERNAME_CUT = 15;
// 100 najlepszych wyników
echo "<h2>Typing Test - Top 100</h3>\n";
$r = mysql_query('
    SELECT date_added, speed, ip, mistakes, corrections,
        pl, chars, minutes, seconds, username
        FROM ttlog WHERE mistakes < 100 ORDER BY speed DESC LIMIT 100');
echo "<table class=\"large-table\">\n<tr>\n";
echo "<th>miejsce</th><th>zakoñczenie pisania</th>\n";
echo "<th>ip</th><th>prêdko¶æ<br>(zn./min)</th>\n";
echo "<th>b³êdy</th><th>poprawno¶æ<br>(%)</th>\n";
echo "<th>polskie<br>znaki</th>\n";
echo "<th>przepisane<br>znaki</th><th>czas</th>\n";
echo "<th>u¿ytkownik</th>\n";
$i = 0;
while ($row = mysql_fetch_assoc($r)) {
    $i++;
    $plTxt = "";
    if ($row['pl'] == 'true') {
        $plTxt = 'tak';
    } else if ($row['pl'] == 'false') {
        $plTxt = 'nie';
    }
    $correctness = compute_correctness(
        $row['chars'], $row['mistakes'], $row['corrections']);
    echo "</tr><tr>\n";
    echo "<td>$i</td><td>" . $row['date_added'] . '</td><td>'
        . $row['ip'] . '</td><td><b>' . $row['speed'] . '</b></td><td>'
        . $row['mistakes'] . '</td><td>' . $correctness . '</td><td>'
        . $plTxt . '</td><td>' . $row['chars'] . '</td><td>'
        . $row['minutes']. ' min ' . $row['seconds'] . ' s</td>'
        . td_cut($row['username'], $USERNAME_CUT) . "\n";
}
echo "</tr>\n</table>\n";
echo "<br>\n\n";

// wszystkie wpisy w ttlog
echo "<h2>Typing Test - Statystyka</h3>\n";
$range = $_GET['range'];
if ($range == 'all') {
    $r = mysql_query('
        SELECT id, date_added, speed, ip, mistakes, corrections,
            pl, chars, minutes, seconds, username
            FROM ttlog
            ORDER BY id DESC
    ');
} else {
    $r = mysql_query('
        SELECT id, date_added, speed, ip, mistakes, corrections,
            pl, chars, minutes, seconds, username
            FROM ttlog
            ORDER BY id DESC
            LIMIT 1000
    ');
}
echo "<table class=\"large-table\">\n<tr>\n";
echo "<th>nr</th><th>zakoñczenie pisania</th>\n";
echo "<th>ip</th><th>prêdko¶æ<br>(zn./min)</th>\n";
echo "<th>b³êdy</th><th>poprawno¶æ<br>(%)</th>\n";
echo "<th>polskie<br>znaki</th>\n";
echo "<th>przepisane<br>znaki</th><th>czas</th>\n";
echo "<th>u¿ytkownik</th>\n";
$i = 0;
while ($row = mysql_fetch_assoc($r)) {
    $i++;
    $plTxt = "";
    if ($row['pl'] == 'true') {
        $plTxt = 'tak';
    } else if ($row['pl'] == 'false') {
        $plTxt = 'nie';
    }
    $correctness = compute_correctness(
        $row['chars'], $row['mistakes'], $row['corrections']);
    echo "</tr><tr>\n";
    echo '<td>' . $row['id'] . '</td><td>' . $row['date_added'] . '</td><td>'
        . $row['ip'] . '</td><td><b>' . $row['speed'] . '</b></td><td>'
        . $row['mistakes'] . '</td><td>' . $correctness . '</td><td>'
        . $plTxt . '</td><td>' . $row['chars'] . '</td><td>'
        . $row['minutes']. ' min ' . $row['seconds'] . ' s</td>'
        . td_cut($row['username'], $USERNAME_CUT) . "\n";
}

?>
</tr>
</table>
<?php
if ($range != 'all') {
    echo "<a href=\"?range=all\">Poka¿ wszystkie</a>\n";
}
?>

</body>
</html>
