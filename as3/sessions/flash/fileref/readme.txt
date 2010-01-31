Orange Flash Uploader v0.5.1
By Donovan Walker

2009.10.25
Built session work around in firefox! Still requires server side parsing of session variable, and knowing what it is... but can be done pretty easily. Example code in demo.

2009.10.22

This is a flexible flash uploader I cobbled together from various examples.
Credit goes to 
	http://www.kirupa.com/developer/actionscript/xmldataflash.htm
	http://www.permadi.com/tutorial/flashVars/index.html
	http://www.actionscript.org/resources/articles/55/1/CSS-in-Flash-MX-2004/Page1.html
	and another  really nice tutorial of the FileReference object that I seem to have misplaced...

Small footprint, configurable via xml. No need to compile it in Flash.
Config file is defaulted to 'config.xml' but can be specified in the FlashVars under 'configURL' as whatever you like.
XML file is not order dependent
Description of XML follows

<security_domain> // domain to set for the security profile
<url_upload> //url to upload the files to (you do need a server side processor!.. upload.php included as an example.
<httpSessionID> //the NAME of the browser's session cookie on the browser side. If provided this will be appended (along with the current session key) to the post url. this was done to work around Flashe's browser session sharing issu in firefox.
<fileFilters> //a list of filters to filter on
	<filter>*.jpg</filter> //some file filters
	<filter>*.gif</filter>
	<filter>*.png</filter>
</fileFilters>
<maxFileBytes>// max size of file in bytes.. make a ridiculous number for large file support
<onProgressJS>//extra javascript to be called while file is uploading
<onCompleteJS>//javascript to be called when file uploading is complete
<progressTextCSS>// define the class 'colors' to control the appearance of the upload % text area
<progressTextDefault> //text that appears in the area reserved for upload % BEFORE upload starts
<progressTextOnComplete> //text that appears in the % area after upload is complete.