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


public class BlockStream{

	public void performDemo(String uri){
		System.out.println("Parsing XML File: "+uri+"\n\n");
		try {
			MyContentHandler contentHandler=new MyContentHandler();
			MyErrorHandler errorHandler=new MyErrorHandler();
			XMLReader parser = XMLReaderFactory.createXMLReader();
			//SAXParser parser=new SAXParser();


			//definiowanie filtru
			BlockStreamFilter filtr=new BlockStreamFilter();
			filtr.setParent(parser);
			filtr.setContentHandler(contentHandler);
			filtr.setErrorHandler(errorHandler);
			filtr.setFeature("http://xml.org/sax/features/validation",false);

			//parsowanie	
			filtr.parse(uri);
			if (filtr.getFeature("http://xml.org/sax/features/validation")){
				System.out.println("Z walidacja");
			}else{
				System.out.println("Bez walidacji");
			}

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
		BlockStream parserDemo=new BlockStream();
		parserDemo.performDemo(uri);
	}
}


class MyErrorHandler implements ErrorHandler{
	public void error(SAXParseException exception) throws SAXException{
		System.out.println("Wystapil blad "+ exception.getMessage());  
		System.out.println("W linii "+exception.getLineNumber());
		System.out.println("W kolumnie "+exception.getColumnNumber());
	}

	public void warning(SAXParseException exception) throws SAXException{
		System.out.println("Wystapilo ostrzezenie "+exception.getMessage());
		System.out.println("W linii "+exception.getLineNumber());
		System.out.println("W kolumnie "+exception.getColumnNumber());
	}

	public void fatalError(SAXParseException exception) throws SAXException{
		System.out.println("Wystapil blad krytyczny "+exception.getMessage());
		System.out.println("W linii "+exception.getLineNumber());
		System.out.println("W kolumnie "+exception.getColumnNumber());
	}
}

class MyContentHandler implements ContentHandler{
	private Locator locator;
	
	public void setDocumentLocator(Locator locator){
		this.locator=locator;
	}

	public void startDocument() throws SAXException{
		System.out.println("ROZPOCZYNAM PARSOWANIE");
	}

	
	public void endDocument() throws SAXException{
		System.out.println("PARSOWANIE ZAKONCZONE");
	}


	public void processingInstruction(String target, String data) throws SAXException{
	}


	public void startPrefixMapping(String prefix,String uri){
	}

	
	public void endPrefixMapping(String prefix){
	}


	
	public void startElement(String namespaceURI,String localName,
				String rawName,Attributes atts)throws SAXException{
		int i;
		System.out.println("\n"+"ELEMENT "+localName);
		System.out.println("... Linia "+locator.getLineNumber());
		System.out.println("...Kolumna "+locator.getColumnNumber());
		System.out.println("...Przestrzen nazw "+namespaceURI);
		System.out.println("...Pelna nazwa "+rawName);

		for (i=0;i<atts.getLength();i++){
			System.out.println(">Atrybut "+atts.getLocalName(i));
			System.out.println(">>>>>>>> Przestrzen nazw "+atts.getURI(i));
			System.out.println(">>>>>>>> Pelna nazwa "+atts.getQName(i));
			System.out.println(">>>>>>>> Typ "+atts.getType(i));
			System.out.println(">>>>>>>> Wartosc "+atts.getValue(i));
		}
				
			
	}
	

	public void endElement(String namespaceURI,String localName,
				String rawName)throws SAXException{
		System.out.print("\n"+localName+">>>\n");
	}



	public void characters(char[] ch,int start,int end)throws SAXException{
		String s=new String(ch,start,end);
		System.out.print("----Zawartosc-----"+s);
	}



	public void ignorableWhitespace(char[] ch,int start,int end)throws SAXException{
	}



	public void skippedEntity(String name)throws SAXException{
		System.out.println("ZNALAZLEM ENCJE");
	}
}	
