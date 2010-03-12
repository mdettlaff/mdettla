<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
  <head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8">
    <meta name="Language" content="pl">
    <title>Szybkie Pisanie na Klawiaturze</title>
  </head>

  <body bgcolor="#CACACA" text="#050505" leftmargin="0" topmargin="0"
      marginheight="0" marginwidth="0">

<?
$pic = $_GET['pic'];

if (!empty($pic)) {
    echo "
<a href=\"#\" onClick=\"window.close()\">
<img src=\"$pic\" border=\"0\" title=\"Zamknij okno\" /></a>
    ";
}
?>

  </body>
</html>
