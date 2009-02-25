newline="NEWLINE"
echo Podaj tytuł wpisu
read title
echo Podaj treść wpisu
while read -r wiersz # czyta po jednej linijce ze standardowego wejscia
  do
    content=$content$newline$wiersz
  done

echo
echo "    <item>"
echo "      <title>$title</title>"
echo -n "      <link>http://queencorner.ovh.org/index.php?go=news#"
echo -n `date +%d-%m-%Y`
echo "</link>"
echo "      <description>"
echo $content | sed 's/\&/\&amp;/g' | sed 's/</\&lt;/g' | sed 's/>/\&gt;/g' \
              | sed 's/^NEWLINE//g' | sed 's/NEWLINE/\n/g'
echo "      </description>"
echo "      <pubDate>`date -R`</pubDate>"
echo "      <category>Newsy o Queen</category>"
echo "      <author>fredmiot@poczta.onet.pl (Daga)</author>"
echo "    </item>"
echo
