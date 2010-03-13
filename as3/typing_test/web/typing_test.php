<html>
  <head>
    <meta http-equiv="Content-type" content="text/html; charset=utf-8">
    <meta name="Language" content="pl">
    <title>Test prędkości online</title>

    <script src="javascript/highscore.js"></script>
    <script src="javascript/AC_OETags.js"></script>

    <style>
      .highscore-table th {
        padding: 5 7 10 7;
        text-align: left;
      }
      .highscore-table td {
        padding: 3 10 3 10;
        text-align: right;
      }
    </style>
  </head>
  <body bgcolor="#F5F5F5">

Poniższy test pozwoli ci na zmierzenie swojej prędkości pisania.
<div>

<!--
Smart developers always View Source.

This application was built using Adobe Flex, an open source framework
for building rich Internet applications that get delivered via the
Flash Player or to desktops via Adobe AIR.

Learn more about Flex at http://flex.org
// -->
<script>
<!--
// -----------------------------------------------------------------------------
// Globals
// Major version of Flash required
var requiredMajorVersion = 9;
// Minor version of Flash required
var requiredMinorVersion = 0;
// Minor version of Flash required
var requiredRevision = 28;
// -----------------------------------------------------------------------------

// Version check for the Flash Player that has the ability to start Player Product Install (6.0r65)
var hasProductInstall = DetectFlashVer(6, 0, 65);

// Version check based upon the values defined in globals
var hasRequestedVersion = DetectFlashVer(requiredMajorVersion, requiredMinorVersion, requiredRevision);

if (hasProductInstall && !hasRequestedVersion) {
    // DO NOT MODIFY THE FOLLOWING FOUR LINES
    // Location visited after installation is complete if installation is required
    var MMPlayerType = (isIE == true) ? "ActiveX" : "PlugIn";
    var MMredirectURL = window.location;
    document.title = document.title.slice(0, 47) + " - Flash Player Installation";
    var MMdoctitle = document.title;

    AC_FL_RunContent(
        "src", "flash/playerProductInstall",
        "FlashVars", "MMredirectURL="+MMredirectURL+'&MMplayerType='+MMPlayerType+'&MMdoctitle='+MMdoctitle+"",
        "width", "675",
        "height", "450",
        "align", "middle",
        "id", "TypingTestApp",
        "quality", "high",
        "bgcolor", "#869ca7",
        "name", "TypingTestApp",
        "allowScriptAccess","sameDomain",
        "type", "application/x-shockwave-flash",
        "pluginspage", "http://www.adobe.com/go/getflashplayer"
    );
} else if (hasRequestedVersion) {
    // if we've detected an acceptable version
    // embed the Flash Content SWF when all tests are passed
    AC_FL_RunContent(
        "src", "flash/TypingTestApp",
        "width", "675",
        "height", "450",
        "align", "middle",
        "id", "TypingTestApp",
        "quality", "high",
        "bgcolor", "#f5f5f5",
        "name", "TypingTestApp",
        "allowScriptAccess","sameDomain",
        "type", "application/x-shockwave-flash",
        "pluginspage", "http://www.adobe.com/go/getflashplayer"
    );
} else { // flash is too old or we can't detect the plugin
    var alternateContent = '<br>Do wyświetlenia strony wymagany jest Adobe Flash Player. '
    + '<a href=http://www.adobe.com/go/getflash/>Pobierz Flash</a>';
    document.write(alternateContent); // insert non-flash content
}
// -->
</script>
<noscript>
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
</noscript>

</div>
<div id="highscoreTableArea"></div>

<script>
updateHighscoreTable();
</script>

  </body>
</html>
