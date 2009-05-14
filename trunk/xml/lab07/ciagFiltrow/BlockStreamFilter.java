import org.xml.sax.*;
import org.xml.sax.helpers.XMLFilterImpl;

public class BlockStreamFilter extends XMLFilterImpl{
	public BlockStreamFilter(){
	}

	public BlockStreamFilter(XMLReader parent){
		super(parent);			
	}

	public void startElement(String uri,String localName,String qName, Attributes atts) throws SAXException {
		if (!localName.equals("html")){
		getContentHandler().startElement(uri,localName,qName,atts);
		}
	}

	public void endElement(String uri, String localName, String qName) throws SAXException {
		
		if (!localName.equals("html")){
		getContentHandler().endElement(uri,localName,qName);
		}
	}
}
		
		
