import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import javax.xml.stream.events.XMLEvent;

public class Run {
	public static void main(String[] args) {

		System.setProperty("javax.xml.stream.XMLInputFactory",
				"com.ctc.wstx.stax.WstxInputFactory");
		XMLInputFactory factory = XMLInputFactory.newInstance();
		

		try {
			XMLStreamReader reader = factory
					.createXMLStreamReader(new FileInputStream(new File(
							"osoba.xml")));

			int eventType = reader.getEventType();
			System.out.println(Run.getEventTypeString(eventType));
			
			while (reader.hasNext()) {
				reader.next();
				eventType = reader.getEventType();
				
				System.out.println(Run.getEventTypeString(eventType));

			}

		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (XMLStreamException e) {
			e.printStackTrace();
		}
	}

	public final static String getEventTypeString(int eventType) {
		switch (eventType) {
		case XMLEvent.START_ELEMENT:
			return "START_ELEMENT";
		case XMLEvent.END_ELEMENT:
			return "END_ELEMENT";
		case XMLEvent.PROCESSING_INSTRUCTION:
			return "PROCESSING_INSTRUCTION";
		case XMLEvent.CHARACTERS:
			return "CHARACTERS";
		case XMLEvent.COMMENT:
			return "COMMENT";
		case XMLEvent.START_DOCUMENT:
			return "START_DOCUMENT";
		case XMLEvent.END_DOCUMENT:
			return "END_DOCUMENT";
		case XMLEvent.ENTITY_REFERENCE:
			return "ENTITY_REFERENCE";
		case XMLEvent.ATTRIBUTE:
			return "ATTRIBUTE";
		case XMLEvent.DTD:
			return "DTD";
		case XMLEvent.CDATA:
			return "CDATA";
		case XMLEvent.SPACE:
			return "SPACE";
		}
		return "UNKNOWN_EVENT_TYPE ,   " + eventType;
	}

}
