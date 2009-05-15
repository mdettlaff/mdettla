import org.xml.sax.Attributes;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.XMLFilterImpl;
import org.xml.sax.helpers.AttributesImpl;

public class ToLowerAttsFilter extends XMLFilterImpl {

	public ToLowerAttsFilter() {}

	public ToLowerAttsFilter(XMLReader parent) {
		super(parent);
	}

	public void startElement(String uri, String localName, String qName,
			Attributes atts) throws SAXException {
		AttributesImpl attsImpl = new AttributesImpl(atts);
		for (int i=0; i < atts.getLength(); i++) {
			attsImpl.setLocalName(i, atts.getLocalName(i).toLowerCase());
			String[] twoPartName = atts.getQName(i).split(":");
			attsImpl.setQName(i,
					twoPartName[0] + ":" + twoPartName[1].toLowerCase());
		}
		super.startElement(uri, localName, qName, attsImpl);
	}

	public String toString() {
		return this.getClass().getName();
	}
}
