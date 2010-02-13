<?php include 'template/top.php' ?>

<!-- 
Smart developers always View Source. 

This application was built using Adobe Flex, an open source framework
for building rich Internet applications that get delivered via the
Flash Player or to desktops via Adobe AIR. 

Learn more about Flex at http://flex.org 
// -->

<h2>Tetris</h2>

<object classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
        id="TetrisApp" width="100%" height="100%"
        codebase="http://fpdownload.macromedia.com/get/flashplayer/current/swflash.cab">
        <param name="movie" value="TetrisApp.swf" />
        <param name="quality" value="high" />
        <param name="bgcolor" value="#869ca7" />
        <param name="allowScriptAccess" value="sameDomain" />
        <embed src="flash/TetrisApp.swf" quality="high" bgcolor="#f0f0f0"
            width="250" height="475" name="TetrisApp" align="middle"
            play="true"
            loop="false"
            quality="high"
            allowScriptAccess="sameDomain"
            type="application/x-shockwave-flash"
            pluginspage="http://www.adobe.com/go/getflashplayer">
        </embed>
</object>

<?php include 'template/bottom.php' ?>
