import java.util.List;
import org.xml.sax.*;
import org.xml.sax.helpers.XMLFilterImpl;

public class SkipFilter extends XMLFilterImpl {
	private List<String> tagsToSkip;

	public SkipFilter(List<String> tagsToSkip) {
		this.tagsToSkip = tagsToSkip;
	}

	public SkipFilter(XMLReader parent, List<String> tagsToSkip) {
		super(parent);
		this.tagsToSkip = tagsToSkip;
	}

	public void startElement(String uri, String localName, String qName,
			Attributes atts) throws SAXException {
		if (!tagsToSkip.contains(localName)) {
			//getContentHandler().startElement(uri, localName, qName, atts);
			super.startElement(uri, localName, qName, atts);
		}
	}

	public void endElement(String uri, String localName, String qName)
		throws SAXException {

		if (!tagsToSkip.contains(localName)) {
			//getContentHandler().endElement(uri, localName, qName);
			super.endElement(uri, localName, qName);
		}
	}

	public String toString() {
		return this.getClass().getName();
	}
}
