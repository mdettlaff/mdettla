import java.util.Map;
import java.util.HashMap;
import org.xml.sax.Attributes;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.XMLFilterImpl;

public class ChangeNamespaceFilter extends XMLFilterImpl{
	Map<String, String> namespaceMappings = new HashMap<String, String>();

	public ChangeNamespaceFilter(Map<String, String> namespaceMappings) {
		this.namespaceMappings = namespaceMappings;
	}

	public ChangeNamespaceFilter(XMLReader parent,
			Map<String, String> namespaceMappings){
		super(parent);
		this.namespaceMappings = namespaceMappings;
	}

	public void startElement(String uri, String localName, String qName,
			Attributes atts) throws SAXException {
		if (namespaceMappings.keySet().contains(uri)) {
			uri = namespaceMappings.get(uri);
		}
		super.startElement(uri, localName, qName, atts);
	}

	public void endElement(String uri, String localName,
			String qName) throws SAXException {
		if (namespaceMappings.keySet().contains(uri)) {
			uri = namespaceMappings.get(uri);
		}
		super.endElement(uri, localName, qName);
	}

	public void startPrefixMapping(String prefix,
			String uri) throws SAXException {
		if (namespaceMappings.keySet().contains(uri)) {
			uri = namespaceMappings.get(uri);
		}
		super.startPrefixMapping(prefix, uri);
	}

	public String toString() {
		return this.getClass().getName();
	}
}
