<html>
  <head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8">
    <meta name="Language" content="pl">
    <title>Test prędkości online</title>

    <script src="javascript/highscore.js"></script>

    <style>
      .highscore-table th {
        padding: 5 7 10 7;
        text-align: left;
      }
      .highscore-table td {
        padding: 3 10 3 10;
        text-align: right;
      }
      .highscore-table td:nth-child(2) {
        text-align: left;
      }
      .highscore-table tr:nth-child(even) {
        background: #F1F1DD
      }
    </style>
  </head>
  <body bgcolor="#F5F5F5">

Poniższy test pozwoli ci na zmierzenie swojej prędkości pisania.<br />

<!--
Smart developers always View Source.

This application was built using Adobe Flex, an open source framework
for building rich Internet applications that get delivered via the
Flash Player or to desktops via Adobe AIR.

Learn more about Flex at http://flex.org
// -->
<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
        id="TypingTest" width="675" height="450"
        codebase="http://fpdownload.macromedia.com/get/flashplayer/current/swflash.cab">
        <param name="movie" value="flash/TypingTestApp.swf" />
        <param name="quality" value="high" />
        <param name="bgcolor" value="#f5f5f5" />
        <param name="allowScriptAccess" value="sameDomain" />
        <embed src="flash/TypingTestApp.swf" quality="high" bgcolor="#f5f5f5"
            width="675" height="450" name="TypingTest" align="middle"
            play="true"
            loop="false"
            allowScriptAccess="sameDomain"
            type="application/x-shockwave-flash"
            pluginspage="http://www.adobe.com/go/getflashplayer">
        </embed>
</object>

<br />
<div id="highscoreTableArea"></div>

<script>
updateHighscoreTable();
</script>

  </body>
</html>
