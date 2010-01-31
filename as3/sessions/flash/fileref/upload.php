<?php
//THIS IS AN INSECURE DEMO! MAKE SURE TO ADD APPROPRIATE SECURITY!

//workaround for firefox. replace 'PHPSESSID' WITH THE VALUE FROM YOUR config.xml's <httpSessionID>
//make sure you include the following 3 lines before you do session start in ANY other file used during the upload.
if(array_key_exists('PHPSESSID', $_GET)) {
	session_id($_GET['PHPSESSID']);
}
session_start();

//example of functional session sharing.. each time you refresh the index page, the counter will skip a number
if(!array_key_exists("counter", $_SESSION)) $_SESSION["counter"] = 1;
$_SESSION["counter"]++;

move_uploaded_file($_FILES['Filedata']['tmp_name'], "./files/". $_SESSION["counter"]  . "_" . $_FILES['Filedata']['name']);
chmod("./files/".$_FILES['Filedata']['name'], 0777); 
?>
