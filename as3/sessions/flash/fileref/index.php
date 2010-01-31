<?php session_start() ; //php here only to increment this counter.
if(!array_key_exists("counter", $_SESSION)) $_SESSION["counter"] = 1;
$_SESSION["counter"]++;
?><html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
<title>fileref</title>
</head>
<body style="background:black; color:white">
<div align="center" style="background:black">
	<object classid="clsid:d27cdb6e-ae6d-11cf-96b8-444553540000" codebase="http://fpdownload.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=8,0,0,0" width="230" height="33" id="fileref" align="middle">
	<param name="allowScriptAccess" value="sameDomain" />
	<param NAME="wmode" Value="Transparent">
	<PARAM NAME="FlashVars" VALUE="configURL=config.xml">
	<param name="movie" value="fileref.swf" /><param name="quality" value="high" /><param name="bgcolor" value="#ffffff" />
	<embed src="fileref.swf" quality="high" bgcolor="#ffffff" 
		width="230" height="33" name="fileref" align="middle" allowScriptAccess="sameDomain" 
		wmode="Transparent" 
		FlashVars="configURL=config.xml"
		type="application/x-shockwave-flash" 
		pluginspage="http://www.macromedia.com/go/getflashplayer" />
	</object>
</div>
</body>
</html>

