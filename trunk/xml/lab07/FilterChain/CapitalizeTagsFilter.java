import org.xml.sax.Attributes;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
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
		super.startElement(uri, localName, qName, atts);
	}

	public void endElement(String uri, String localName,
			String qName) throws SAXException {
		localName = localName.toUpperCase();
		String[] twoPartName = qName.split(":");
		qName = twoPartName[0] + ":" + twoPartName[1].toUpperCase();
		super.endElement(uri, localName, qName);
	}

	public String toString() {
		return this.getClass().getName();
	}
}
