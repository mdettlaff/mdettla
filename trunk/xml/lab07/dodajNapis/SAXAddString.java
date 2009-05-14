import java.io.IOException;
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
//import org.apache.xerces.parsers.SAXParser;

public class SAXAddString{

	public void performDemo(String uri){
		System.out.println("Parsing XML File: "+uri+"\n\n");
		try {
			XMLReader parser = XMLReaderFactory.createXMLReader();
			//SAXParser parser=new SAXParser();
			
			AddStringFilter filtr=new AddStringFilter();
			filtr.setParent(parser);
			filtr.parse(uri);

		}
		catch(IOException e){System.out.println("Input/Output exception "+e.getMessage());}
		catch(SAXNotRecognizedException e){System.out.println("SAX Not Recognized Exception "+e.getMessage());}
		catch(SAXNotSupportedException e){System.out.println("SAX Not Supported Exception "+e.getMessage());}
		catch(SAXParseException e){System.out.println("SAX Parse Exception "+e.getMessage());}

		catch(SAXException e){System.out.println("SAX Exception "+e.getMessage());}
			
	}	

	public static void main(String[] args){

		if (args.length!=1){
			System.out.println("Usage : java SAXParserDemo [XML URI]");
			System.exit(0);
		}
		String uri=args[0];
		SAXAddString parserDemo=new SAXAddString();
		parserDemo.performDemo(uri);
	}
}


