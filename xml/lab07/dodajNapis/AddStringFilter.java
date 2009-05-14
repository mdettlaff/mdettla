import org.xml.sax.Attributes; 
import org.xml.sax.SAXException; 
import org.xml.sax.helpers.XMLFilterImpl; 
 
public class AddStringFilter extends XMLFilterImpl { 
 
    public void startElement(String uri,  
                             String localName,  
                             String qName, 
                             Attributes atts) throws SAXException { 
        // Map postalcode to postcode 
       if ((localName.equals("postcalcode")) ||
           (qName.equals("postalcode"))) { 
              System.out.println("SXF: " + qName); 
              qName = "postcode"; 
              localName = "postcode"; 
       } 
       // Delegate on to inherited behaviour 
       super.startElement(uri, localName, qName, atts); 
    } 
 
    public void endElement(String uri, String localName, String qName) 
            throws SAXException { 
       // Map postalcode to postcode 
       if ((localName.equals("postcalcode")) ||
           (qName.equals("postalcode"))) { 
              System.out.println("SXF: " + qName); 
              qName = "postcode"; 
              localName = "postcode"; 
       } 
       super.endElement(uri, localName, qName); 
    } 
 
}
