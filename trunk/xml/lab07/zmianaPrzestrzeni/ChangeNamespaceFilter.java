import org.xml.sax.*;
import org.xml.sax.helpers.XMLFilterImpl;

public class ChangeNamespaceFilter extends XMLFilterImpl{
	public ChangeNamespaceFilter(){
	}

	public ChangeNamespaceFilter(XMLReader parent){
		super(parent);			
	}

	public void startElement(String uri,String localName,String qName, Attributes atts) throws SAXException {
		if (uri.equals("http://moja.przestrzen")){
			uri="http://nowa.przestrzen.nazw";
		}
		//getContentHandler().startElement(uri,localName,qName,atts);
		super.startElement(uri,localName,qName,atts);
	}

	public void endElement(String uri, String localName, String qName) throws SAXException {
		
		if (uri.equals("http://moja.przestrzen")){
			uri="http://nowa.przestrzen.nazw";
		}
		//getContentHandler().endElement(uri,localName,qName);
		super.endElement(uri,localName,qName);
	}
}
		
		
