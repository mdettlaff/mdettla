import org.xml.sax.Attributes;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.XMLFilterImpl;

public class IndentFilter extends XMLFilterImpl {

	private final String INDENT = "    ";
	private String currentIndentation;
	private String lastTag;
	private String lastCharacters;

	public IndentFilter() {
		currentIndentation = "";
	}

	public IndentFilter(XMLReader parent) {
		super(parent);
		currentIndentation = "";
	}

	public void startElement(String uri, String localName, String qName,
			Attributes atts) throws SAXException {
		lastTag = qName;
		lastCharacters = "";
		System.out.print(currentIndentation);
		currentIndentation += INDENT;
		super.startElement(uri, localName, qName, atts);
		System.out.println();
	}

	public void endElement(String uri, String localName,
			String qName) throws SAXException {
		if (qName.equals(lastTag)) {
			System.out.print(currentIndentation);
			System.out.println(lastCharacters);
		}
		currentIndentation = currentIndentation.substring(
				0, currentIndentation.length() - INDENT.length());
		System.out.print(currentIndentation);
		super.endElement(uri, localName, qName);
		System.out.println();
	}

	public void characters(char[] ch, int start, int end) throws SAXException {
		lastCharacters += new String(ch, start, end);
	}

	public String toString() {
		return this.getClass().getName();
	}
}
