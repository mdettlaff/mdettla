import org.xml.sax.*;
import org.xml.sax.helpers.XMLFilterImpl;

public class CapitalizeTagsFilter extends XMLFilterImpl {

	public CapitalizeTagsFilter() {}

	public CapitalizeTagsFilter(XMLReader parent) {
		super(parent);
	}

	public void startElement(String uri, String localName, String qName,
			Attributes atts) throws SAXException {
		localName = localName.toUpperCase();
		String[] twoPartName = qName.split(":");
		qName = twoPartName[0] + ":" + twoPartName[1].toUpperCase();
		//getContentHandler().startElement(uri, localName, qName, atts);
		super.startElement(uri, localName, qName, atts);
	}

	public void endElement(String uri, String localName,
			String qName) throws SAXException {
		localName = localName.toUpperCase();
		String[] twoPartName = qName.split(":");
		qName = twoPartName[0] + ":" + twoPartName[1].toUpperCase();
		//getContentHandler().endElement(uri, localName, qName);
		super.endElement(uri, localName, qName);
	}

	public String toString() {
		return this.getClass().getName();
	}
}
