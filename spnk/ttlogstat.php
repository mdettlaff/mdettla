<?
mysql_connect();

echo "<html>\n<body>\n";

// wykres slupkowy z predkosciami
$maxspeed = 400;
for ($i=0; $i < $maxspeed/50; $i++) {
	$xv = $i*50;
	$r = mysql_query("SELECT COUNT(`speed`) AS scount FROM `ttlog`
		WHERE `speed` > ".$xv." AND `speed` < ".($xv+50)."");
	$row = mysql_fetch_array($r);
	$data[$i] = $row["scount"];
}
$r = mysql_query("SELECT COUNT(`speed`) AS scount FROM `ttlog`
	WHERE `speed` > ".($i*50)."");
$row = mysql_fetch_array($r);
$data[$i] = $row["scount"];

$sum = array_sum($data);
$height = 320;
$x = 25;
$y = 0;
echo "<table border=0>\n<tr>\n";
for ($i=0; $i < $maxspeed/50+1; $i++){
	echo "<td align=center valign=bottom width=40>";
	$y = ($data[$i]/$sum)* $height;
	$perc = $data[$i]/$sum * 100;
	printf("%.0f%%", $perc);
	$yint = (int) $y;
	if ($yint > 0) echo "<img src=\"px.gif\" width=$x height=$yint>";
	$xvalue = $i*50;
	echo "<br><small>".$xvalue."-";
	if ($xvalue < $maxspeed) echo $xvalue+50; else echo "...";
	echo "</small>";
	echo "</td>\n";
}
echo "</tr>\n</table>\n";
echo "<br>\n";

// statystyka pisania z polskimi znakami
$r = mysql_query("SELECT COUNT(`pl`) AS plON FROM `ttlog` WHERE `pl` = 'true'");
$row = mysql_fetch_array($r);
$plCharsON = (double) $row["plON"];
$r = mysql_query("SELECT COUNT(`pl`) AS plOFF FROM `ttlog` WHERE `pl` = 'false'");
$row = mysql_fetch_array($r);
$plCharsOFF = (double) $row["plOFF"];
$plCharsSUM = $plCharsON + $plCharsOFF;
echo "<table cellspacing=0 cellpadding=0 border=0>\n<tr>";
echo "<td colspan=4 height=25 valign=top><b>Polskie znaki w³±czone:</b></td></tr>\n<tr>\n";
printf("<td>tak %.0f%%&nbsp;</td><td width=%.0f bgcolor=#00FF00>&nbsp;</td>\n",
	$plCharsON / $plCharsSUM * 100, $plCharsON / $plCharsSUM * 275);
printf("<td width=%.0f bgcolor=#FF0000>&nbsp;</td><td>&nbsp;%.0f%% nie</td>\n",
	$plCharsOFF / $plCharsSUM * 275, $plCharsOFF / $plCharsSUM * 100);
echo "</tr>\n</table>\n<br>\n";

// obliczajac srednia uwzgledniamy tylko predkosci wieksze niz 50
$r = mysql_query('SELECT AVG(`speed`) AS average_speed FROM `ttlog` WHERE `speed` > 50');
$row = mysql_fetch_array($r);
$mean = $row["average_speed"];
printf("<big>¦rednia prêdko¶æ: <b>%.1f</b> znaków/min</big><br>\n", $mean);
echo "<hr width=\"400\" align=\"left\">\n";
echo "<h2>Typing Test - Top 100</h3>\n";
$r = mysql_query('SELECT `date_added`, `speed`, `ip`, `mistakes`, `pl`, `chars`, `minutes`, `seconds`
	FROM `ttlog` WHERE `mistakes` < 100 ORDER BY `speed` DESC LIMIT 100');
$tab3 = "&nbsp;&nbsp;&nbsp;";
echo "<table cellpadding=2>\n<tr bgcolor=#D8D8D8>\n";
echo "<td><b>miejsce".$tab3."</b></td><td><b>zakoñczenie pisania".$tab3."</b></td>\n";
echo "<td><b>ip</b></td><td><b>prêdko¶æ".$tab3."<br>(zn./min)</b></td>\n";
echo "<td><b>b³êdy".$tab3."</b></td><td><b>polskie".$tab3."<br>znaki</b></td>\n";
echo "<td><b>przepisane<br>znaki</b></td><td><b>czas</b></td>\n";
$i = 0;
while($row = mysql_fetch_assoc($r)){
	$i++;
	$plTxt="";
	if ($row['pl'] == "true") { $plTxt="tak"; }
	elseif ($row['pl'] == "false") { $plTxt="nie"; }
	if ($i % 2 == 0) { echo "</tr><tr bgcolor=#E8E8E8>\n"; }
	else { echo "</tr><tr>\n"; }
	echo '<td>'.$i.'</td><td>'.$row['date_added'].'</td><td>'.$row['ip'].
	'</td><td><b>'.$row['speed'].'</b></td><td>'.$row['mistakes'].
	'</td><td>'.$plTxt.'</td><td>'.$row['chars'].'</td><td>'.$row['minutes'].
	' min '.$row['seconds'].' s</td>'."\n";
}
echo "</tr>\n</table>\n";
echo "<br>\n";

echo "<h2>Typing Test - Statystyka</h3>\n";
$r = mysql_query('SELECT `id`, `date_added`, `speed`, `ip`, `mistakes`, `pl`, `chars`,
	`minutes`, `seconds` FROM `ttlog` ORDER BY `id` DESC');
$tab3 = "&nbsp;&nbsp;&nbsp;";
echo "<table cellpadding=2>\n<tr bgcolor=#D8D8D8>\n";
echo "<td><b>nr</b></td><td><b>zakoñczenie pisania".$tab3."</b></td>\n";
echo "<td><b>ip</b></td><td><b>prêdko¶æ".$tab3."<br>(zn./min)</b></td>\n";
echo "<td><b>b³êdy".$tab3."</b></td><td><b>polskie".$tab3."<br>znaki</b></td>\n";
echo "<td><b>przepisane<br>znaki</b></td><td><b>czas</b></td>\n";
$i = 0;
while($row = mysql_fetch_assoc($r)){
	$i++;
	$plTxt="";
	if ($row['pl'] == "true") { $plTxt="tak"; }
	elseif ($row['pl'] == "false") { $plTxt="nie"; }
	if ($i % 2 == 0) { echo "</tr><tr bgcolor=#E8E8E8>\n"; }
	else { echo "</tr><tr>\n"; }
	echo '<td>'.$row['id'].'</td><td>'.$row['date_added'].'</td><td>'.$row['ip'].
	'</td><td><b>'.$row['speed'].'</b></td><td>'.$row['mistakes'].
	'</td><td>'.$plTxt.'</td><td>'.$row['chars'].'</td><td>'.$row['minutes'].
	' min '.$row['seconds'].' s</td>'."\n";
}
echo "</tr>\n</table>\n";

echo "<br>&nbsp;\n";
echo "</body>\n</html>\n";

mysql_close();

?>
