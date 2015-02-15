<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN">
<html>
<head>
 <META HTTP-EQUIV="Content-type" CONTENT="text/html; charset=iso-8859-2">
 <META NAME="Language" CONTENT="pl">
<LINK REL="stylesheet" href="lay/style.css" TYPE="text/css">
<title>Queen Corner online - galeria zdjęć</title>
</head>

<body bgcolor=#cacaca text=#050505 leftmargin=0 topmargin=0 marginheight=0 marginwidth=0>

<?
$fot = $_GET['fot'];
$kom = $_GET['kom'];

if($fot)
{
echo "
<a href=\"javascript:window.close();\">
<img src=\"$fot\" border=0 title=\"Zamknij okno\" /></a>
";
if ($kom[$a]!="")
{
echo "
&nbsp;<br>
&nbsp; $kom
";
}
}
?>

</body>

</html>

