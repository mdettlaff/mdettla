import flash.net.FileReference;
import flash.external.ExternalInterface;


//VARIABLES DEFINED HERE BECAUSE ACTIONSCRIPT MIGHT BE A SINGLE PASS COMPILER.
var fileTypes:Array = new Array();
var imageTypes:Object = new Object();
var my_xml = new XML();
my_xml.ignoreWhite = true;

var fileListener:Object = new Object();
var btnListener:Object = new Object();
var maxFileBytes:Number = 0;
var uploadURL:String = "";
var onProgressJS:String = "";
var onCompleteJS:String = "";
var onCompleteText:String = "Done";
var sessionName:String = "";
var sessionCookie:String = "";

/*if configURL not specified in embed tags. use the default configuration  file*/
if(_root.configURL == undefined) {
	configURL = "config.xml";
}

/*upload progress is a ui text box*/
uploadProgress.styleSheet = new TextField.StyleSheet();
uploadProgress.text = "";


//FUNCTIONS AND EVENT HANDLERS

/*convenience function for making sure the upload progress text has class 'colors'*/
function uploadText(inText:String) {
	uploadProgress.text = "<span class='colors'>" + inText + "</span>";
}

function getCookie(cookieName) {
    var r = "";
    var search = cookieName + "=";
    var js = "function flash_get_cookie(){ return document.cookie;}"
    var cookieVariable = ExternalInterface.call(js).toString();
    
    if(cookieVariable.length > 0) {
        offset = cookieVariable.indexOf(search);
        if (offset != -1) {
            offset += search.length;
            end = cookieVariable.indexOf(";", offset);
            if (end == -1) end = cookieVariable.length;
            r = unescape(cookieVariable.substring(offset, end));
        }
    }
    return r;
}

/*set all of our config variables*/
function xmlOnLoad(success) {
	if (success)
	{
		System.security.allowDomain(this.childNodes[0].firstChild.nodeValue);
		fileTypes = new Array();
		imageTypes = new Object();
		imageTypes.description = "Types ("
		imageTypes.extension = "";
		/*loop through all the root nodes discovering name as we go. This allows the xml file to be non-order dependant*/
		var names = "";
		for(j = 0; j < this.childNodes.length; j++) {			
			var node 	 = this.childNodes[j];
			names += node.nodeName + "; ";
			switch(node.nodeName) {
				case "url_upload" :
					uploadURL = node.firstChild.nodeValue;
					trace(uploadURL);
					break;
				case "onProgressJS" :
					onProgressJS = node.firstChild.nodeValue;
					break;
				case "onCompleteJS" :
					onCompleteJS = node.firstChild.nodeValue;		
					trace(onCompleteJS);
					break;
				case "maxFileBytes" :
					maxFileBytes = node.firstChild.nodeValue;
					break;
				case "progressTextCSS" :
					uploadProgress.styleSheet.parseCSS(node.firstChild.nodeValue);
					trace(node.firstChild.nodeValue);
					break;
				case "progressTextDefault" :
					uploadText(node.firstChild.nodeValue);
					break;
				case "progressTextOnComplete" :
					onCompleteText = node.firstChild.nodeValue;
					break;
				case "fileFilters" :
					for(i = 0; i < node.childNodes.length; i++) {
						imageTypes.description += node.childNodes[i].firstChild.nodeValue + ",";
						imageTypes.extension += node.childNodes[i].firstChild.nodeValue + "; ";
					}
					break;
				case "security_domain" :
					System.security.allowDomain(node.firstChild.nodeValue);
					break;
				case "httpSessionID" :
					sessionName = node.firstChild.nodeValue;
					
					break;
			}
		}
		if(sessionName.length > 0) {
			sessionCookie = getCookie(sessionName);
			if( uploadURL.indexOf("?") > -1) {
				uploadURL += "&";
			} else {
				uploadURL += "?";
			}
			uploadURL += sessionName + "=" + sessionCookie;
		}
		imageTypes.description += ")";
		fileTypes.push(imageTypes);
	} else {
		getURL("javascript:alert('Could not load the config file (" + configURL + ").');");
	}

	
}


btnListener.click = function(eventObj:Object)
{
    var fileRef:FileReference = new FileReference();
	fileRef.addListener(fileListener);
	fileRef.browse(fileTypes);
}
uploadButton.addEventListener("click", btnListener);

fileListener.onCancel = function(file:FileReference):Void
{
    uploadText("Upload Cancelled");
}

/*once you've chosen a file from the file dialog, do the following*/
fileListener.onSelect = function(file:FileReference):Void
{
	trace(maxFileBytes + " < "+ file.size);
	/*if filesize too big abort upload and notify user*/
	if(maxFileBytes < file.size) {
		var size = Math.round(file.size /(1048576 / 100)) /100;
		var maxSize = Math.round(maxFileBytes /(1048576 / 100)) /100;
		var megabytes = "Megabyte";
		if(maxSize > 1) megabytes +="s";
		getURL("javascript:alert('This file is too large. The maximum file size is " + maxSize + " " + megabytes + "');");
	} else {
		if(!file.upload(uploadURL))
		{
			getURL("javascript:alert('Upload dialog failed to open. Contact Support');");
		}
	}
}

fileListener.onOpen = function(file:FileReference):Void
{
}

fileListener.onProgress = function(file:FileReference, bytesLoaded:Number, bytesTotal:Number):Void
{
	if(onProgressJS.length > 0) {
				getURL("javascript:" + onProgressJS);
	}
	uploadText("Uploading: " + Math.round((bytesLoaded / file.size) * 100) + "%");
}

fileListener.onComplete = function(file:FileReference):Void
{
	if(onCompleteJS.length > 0) {
			getURL("javascript:" + onCompleteJS);
	}
	uploadText(onCompleteText);
}

fileListener.onHTTPError = function(file:FileReference, errorString:String):Void
{
    uploadText("HTTP Error: " + errorString);
}

fileListener.onIOError = function(file:FileReference):Void
{
    uploadText("IO Error: " + file.name);
}

fileListener.onSecurityError = function(file:FileReference, errorString:String):Void
{
    uploadText("Security Error: " + errorString);
}


/*assign the onLoad handler and load the xml configuration. (this is the thing that happens right after the variables are assigned) */
my_xml.onLoad = xmlOnLoad;
my_xml.load(configURL);
