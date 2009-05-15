import java.io.IOException;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.ErrorHandler;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.Attributes;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.helpers.XMLFilterImpl;

import gnu.getopt.Getopt;
//import org.apache.xerces.parsers.SAXParser;

public class FilterChain {

	public void filter(String uri, List<XMLFilterImpl> filters) {
		try {
			MyContentHandler contentHandler = new MyContentHandler();
			MyErrorHandler errorHandler = new MyErrorHandler();
			XMLReader parser = XMLReaderFactory.createXMLReader();
			//SAXParser parser = new SAXParser();

			XMLReader filter; // pierwszy filtr
			if (filters.size() > 0) {
				filters.get(0).setParent(parser);
				for (int i=1; i < filters.size(); i++) {
					filters.get(i).setParent(filters.get(i-1));
				}
				filter = filters.get(filters.size() - 1);
			} else {
				filter = parser;
			}
			filter.setContentHandler(contentHandler);
			filter.setErrorHandler(errorHandler);
			filter.setFeature(
					"http://xml.org/sax/features/validation", false);
			filter.parse(uri);
		}
		catch (IOException e) {
			System.out.println("Input/Output exception " + e.getMessage());
		}
		catch (SAXNotRecognizedException e) {
			System.out.println(
					"SAX Not Recognized Exception " + e.getMessage());
		}
		catch (SAXNotSupportedException e) {
			System.out.println("SAX Not Supported Exception " + e.getMessage());
		}
		catch (SAXParseException e) {
			System.out.println("SAX Parse Exception " + e.getMessage());
		}
		catch (SAXException e) {
			System.out.println("SAX Exception " + e.getMessage());
		}
	}

	public static void main(String[] args){
		List<XMLFilterImpl> filters = new ArrayList<XMLFilterImpl>();
		Getopt getopt = new Getopt("FilterChain", args, "tain:s:");
		int c;
		String arg;
		while ((c = getopt.getopt()) != -1) {
			switch(c) {
				case 't':
					filters.add(new CapitalizeTagsFilter());
					break;
				case 'a':
					filters.add(new ToLowerAttsFilter());
					break;
				case 'i':
					filters.add(new IndentFilter());
					break;
				case 's':
					arg = getopt.getOptarg();
					filters.add(new SkipFilter(Arrays.asList(arg.split(" "))));
					break;
				case 'n':
					arg = getopt.getOptarg();
					String[] optargs = arg.split(" ");
					Map<String, String> nsMappings =
						new HashMap<String, String>();
					for (int i=0; i < optargs.length; i+=2) {
						nsMappings.put(optargs[i], optargs[i+1]);
					}
					filters.add(new ChangeNamespaceFilter(nsMappings));
					break;
				case '?':
					System.exit(2);
			}
		}
		if (getopt.getOptind() < args.length) {
			String uri = args[getopt.getOptind()];
			FilterChain filterChain = new FilterChain();
			System.out.println("Parsowanie pliku " + uri);
			System.out.println("Użyte filtry: " + filters + "\n");
			filterChain.filter(uri, filters);
		} else {
			System.out.println(
					"Użycie: java FilterChain [opcje] URI\n" +
					"Opcje:\n" +
					"  -a Filtr zamieniający litery na małe w atrybutach.\n" +
					"  -i Filtr formatujący wcięcia w dokumencie.\n" +
					"  -n NSMAPPINGS Filtr zamieniający przestrzenie nazw.\n" +
					"  -s TAGS Filtr pomijający podane znaczniki.\n" +
					"  -t Filtr zamieniający litery na wielkie w znacznikach."
					);
			System.exit(2);
		}
	}
}


class MyErrorHandler implements ErrorHandler{
	public void error(SAXParseException e) throws SAXException {
		System.out.println("Wystąpil błąd " + e.getMessage());
		System.out.println("W linii " + e.getLineNumber());
		System.out.println("W kolumnie " + e.getColumnNumber());
	}

	public void warning(SAXParseException e) throws SAXException {
		System.out.println("Wystąpiło ostrzeżenie " + e.getMessage());
		System.out.println("W linii " + e.getLineNumber());
		System.out.println("W kolumnie " + e.getColumnNumber());
	}

	public void fatalError(SAXParseException e) throws SAXException {
		System.out.println("Wystąpił błąd krytyczny " + e.getMessage());
		System.out.println("W linii " + e.getLineNumber());
		System.out.println("W kolumnie " + e.getColumnNumber());
	}
}


class MyContentHandler implements ContentHandler{
	private String prefix;
	private String uri;
	private Locator locator;

	public void setDocumentLocator(Locator locator) {
		this.locator = locator;
	}

	public void startDocument() throws SAXException {}

	public void endDocument() throws SAXException {
		System.out.println();
	}

	public void processingInstruction(String target, String data)
		throws SAXException {}

	public void startPrefixMapping(String prefix, String uri) {
		this.prefix = prefix;
		this.uri = uri;
	}

	public void endPrefixMapping(String prefix) {}

	public void startElement(String namespaceURI, String localName,
				String rawName, Attributes atts) throws SAXException {
		int i;
		System.out.print("<" + rawName);

		for (i=0; i < atts.getLength(); i++) {
			System.out.print(" " + atts.getQName(i));
			System.out.print("=\"" + atts.getValue(i) + "\"");
		}
		if (prefix != null && uri != null) {
			System.out.print(" xmlns:" + prefix + "=\"" + uri + "\"");
			prefix = null;
			uri = null;
		}
		System.out.print(">");
	}

	public void endElement(String namespaceURI,String localName,
				String rawName) throws SAXException {
		System.out.print("</" + rawName + ">");
	}

	public void characters(char[] ch, int start, int end) throws SAXException {
		String s = new String(ch, start, end);
		System.out.print(s);
	}

	public void ignorableWhitespace(char[] ch, int start, int end)
		throws SAXException {}

	public void skippedEntity(String name) throws SAXException {}
}
