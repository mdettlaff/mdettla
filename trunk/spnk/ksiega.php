<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
  <head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8">
    <meta NAME="Language" content="pl">
    <meta NAME="Author" content="Michał Dettlaff">
    <meta NAME="Copyright" content="Michał Dettlaff 2010">
    <link rel="stylesheet" href="style.css" type="text/css">
    <title>Szybkie Pisanie na Klawiaturze</title>
  </head>

  <body link="#7777EE" alink="#7777EE" vlink="#7777EE" bgcolor="#F5F5F5" text="#000001"
      leftmargin="0" topmargin="0" marginheight="0" marginwidth="0">

    <table border="0" cellpadding="0" cellspacing="0" width="100%">
      <tr>
        <td width="260" valign="top">

          <table align="left" border="0" cellpadding="0" cellspacing="0">
            <tr>
              <td rowspan="2" width="32" height="350" valign="top" background="menu1.jpg"
                  bgcolor="#F5F5F5" style="background-repeat: no-repeat">
              </td>
              <td width="228" height="112" valign="top" background="menu2a.jpg"
                  bgcolor="#F5F5F5" style="background-repeat: no-repeat">
              </td>
            </tr>
            <tr>
              <td width="228" height="238" valign="top" background="menu2b.jpg"
                  bgcolor="#f5f5f5" style="background-repeat: no-repeat">
                <a href="index.html" class="menu">Strona główna</a><br />&nbsp;<br />
                <a href="nauka.htm" class="menu">Jak nauczyć się<br />
                  szybkiego pisania?</a><br />&nbsp;<br />
                <a href="programy.php" class="menu">Programy</a><br />&nbsp;<br />
                <a href="test.htm" class="menu">Test prędkości online</a><br />&nbsp;<br />
                <a href="linki.htm" class="menu">Linki</a><br />&nbsp;<br />
                <a href="ksiega.php" class="menu">Księga gości</a><br />&nbsp;<br />
                <a href="autor.htm" class="menu">Autor strony</a>
              </td>
            </tr>
          </table>

        </td>
        <td valign="top">

          <table width="100%" border="0" cellpadding="0" cellspacing="0">
            <tr>
              <td valign="top" background="menu3.jpg" bgcolor="#F5F5F5"
                  style="background-repeat: no-repeat">

<p>
&nbsp;<br>
<table width="100%">
  <tr bgcolor="#bac5f8" height="30">
    <td><p class=tytulb>&nbsp; &nbsp;KSIĘGA GOŚCI</p></td>
  </tr>
</table>
<table width="100%">
  <tr>
    <td>
      <p>Jeśli chcesz wyrazić swoją opinię o tej stronie lub na temat
          z nią związany - wpisz się!</p>
    </td>
  </tr>
</table>

<form action="" method="POST">
  <table width="100%" cellspacing="0" cellpadding="2">
    <tr>
      <td align="right"><b>Imię:</b></td>
      <td width="95%"><input type="text" name="imie"><br></td>
    </tr>
    <tr>
      <td align="right"><b>email:</b></td>
      <td><input type="text" name="email"><br></td>
    </tr>
    <tr>
      <td align="right" valign="top"><b>Treść&nbsp;wpisu:</b></td>
      <td><textarea name="tresc" rows=2 cols=50 wrap="physical"></textarea><br></td>
    </tr>
    <tr bgcolor="#bac5f8" height=30>
      <td></td>
      <td>
        <input type="submit" name="zapisz" value="     zapisz      ">&nbsp;&nbsp;
        <input type="reset" name="wyczysc" value="     wyczyść     ">
      </td>
    </tr>
  </table>
</form>

<?php

//echo "POST parameters: " . print_r($_POST, true);
//echo "<br /><br />\n";

pg_connect("dbname=mdettla user=mdettla password=tommyemmanuel");

$result = pg_query("
    SELECT date_added, username, email, content
        FROM guestbook
        ORDER BY date_added DESC
        LIMIT 25
");
if ($result) {
    while ($row = pg_fetch_assoc($result)) {
        $date = str_replace(' 00:00:00', '', $row['date_added']);
        if (!empty($row['email'])) {
            echo "<a href=\"mailto:" . $row['email'] . "\"><b>";
            echo $row['username'] . "</b></a>\n";
        } else {
            echo '<b>' . $row['username'] . '</b> ';
        }
        echo "napisał(a) dnia <b>" . $date;
        echo "</b>\n<br /><br />\n";
        echo str_replace("\n", "<br>\n", $row['content']);
        echo "\n<br /><br />\n<hr />\n\n";
    }
}

?>

<br />
<a href="ksiega_old.html">Zobacz starsze wpisy</a>
<br /><br />

<br />&nbsp;
</p>

            </td>
            <td width="25"></td>
            </tr>
          </table>

        </td>
      </tr>
    </table>

  </body>
</html>
