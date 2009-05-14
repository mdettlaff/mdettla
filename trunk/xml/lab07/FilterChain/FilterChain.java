import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.ErrorHandler;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.Attributes;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.XMLReaderFactory;
import org.xml.sax.helpers.XMLFilterImpl;
//import org.apache.xerces.parsers.SAXParser;


public class FilterChain {

	public void filter(String uri, List<XMLFilterImpl> filters) {
		System.out.println("Parsing XML File: " + uri);
		System.out.println("Filters used: " + filters + "\n");
		try {
			MyContentHandler contentHandler = new MyContentHandler();
			MyErrorHandler errorHandler = new MyErrorHandler();
			XMLReader parser = XMLReaderFactory.createXMLReader();
			//SAXParser parser = new SAXParser();

			//definiowanie filtrów
			filters.get(0).setParent(parser);
			for (int i=1; i < filters.size(); i++) {
				filters.get(i).setParent(filters.get(i-1));
			}
			XMLFilterImpl lastFilter = filters.get(filters.size() - 1);
			lastFilter.setContentHandler(contentHandler);
			lastFilter.setErrorHandler(errorHandler);
			lastFilter.setFeature(
					"http://xml.org/sax/features/validation", false);

			/*CapitalizeTagsFilter capitalize = new CapitalizeTagsFilter();
			capitalize.setParent(parser);
			SkipFilter skip = new SkipFilter(Arrays.asList("wibble", "td"));
			skip.setParent(capitalize);
			skip.setContentHandler(contentHandler);
			skip.setErrorHandler(errorHandler);
			skip.setFeature("http://xml.org/sax/features/validation", false);*/

			//parsowanie
			lastFilter.parse(uri);
			/*if (filter.getFeature("http://xml.org/sax/features/validation")) {
				System.out.println("Z walidacją");
			} else {
				System.out.println("Bez walidacji");
			}*/
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
		if (args.length == 0){
			System.out.println("Usage: java FilterChain XML_URI");
			System.exit(0);
		}
		String uri = args[0];
		List<XMLFilterImpl> filters = Arrays.asList(
				new SkipFilter(Arrays.asList("wibble", "td")),
				new CapitalizeTagsFilter(),
				new ToLowerAttsFilter(),
				new ChangeNamespaceFilter(new HashMap<String, String>() {{
					put("http://moja.przestrzen", "http://nowa.przestrzen");
					put("http://jakas.przestrzen", "http://inna.przestrzen");
				}})
		);
		FilterChain filterChain = new FilterChain();
		filterChain.filter(uri, filters);
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
		//System.out.println("\n"+"ELEMENT "+localName);
		//System.out.println("... Linia "+locator.getLineNumber());
		//System.out.println("...Kolumna "+locator.getColumnNumber());
		//System.out.println("...Przestrzen nazw "+namespaceURI);
		//System.out.println("...Pelna nazwa "+rawName);

		for (i=0; i < atts.getLength(); i++) {
			System.out.print(" " + atts.getQName(i));
			System.out.print("=\"" + atts.getValue(i) + "\"");
			//System.out.println(atts.getLocalName(i));
			//System.out.println(">>>>>>>> Przestrzen nazw "+atts.getURI(i));
			//System.out.println(">>>>>>>> Pelna nazwa "+atts.getQName(i));
			//System.out.println(">>>>>>>> Typ "+atts.getType(i));
			//System.out.println(">>>>>>>> Wartosc "+atts.getValue(i));
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

	public void skippedEntity(String name) throws SAXException {
		System.out.println("Encja");
	}
}
