import java.util.List;
import org.xml.sax.Attributes;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
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
			super.startElement(uri, localName, qName, atts);
		}
	}

	public void endElement(String uri, String localName, String qName)
		throws SAXException {

		if (!tagsToSkip.contains(localName)) {
			super.endElement(uri, localName, qName);
		}
	}

	public String toString() {
		return this.getClass().getName();
	}
}
