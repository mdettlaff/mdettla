import javax.xml.stream.*;
import java.net.URL;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.ArrayList;

public class MergeEmpty {

	public static void main(String[] args) {

		if (args.length < 2) {
			System.err.println("Usage: java MergeEmpty input.xml output.xml");
			return;
		}
		String inputFile = args[0];
		String outputFile = args[1];

		try {
			FileInputStream in = new FileInputStream(inputFile);
			XMLInputFactory factory = XMLInputFactory.newInstance();
			XMLStreamReader parser = factory.createXMLStreamReader(in);

			OutputStream out = new FileOutputStream(outputFile);
			XMLOutputFactory outputFactory = XMLOutputFactory.newInstance();
			XMLStreamWriter writer = outputFactory.createXMLStreamWriter(out);

			Integer prevEvent = null;
			String prevName = "";
			String prevText = "";
			List<String> prevAtts = new ArrayList<String>();
			while (parser.hasNext()) {
				int event = parser.next();
				if (event == XMLStreamConstants.END_ELEMENT
						&& prevEvent == XMLStreamConstants.START_ELEMENT) {
					// jeśli trafiliśmy na parę: <znacznik></znacznik>
					writer.writeEmptyElement(prevName);
					event = parser.next();
				} else if (prevEvent != null) {
					switch (prevEvent) {
						case XMLStreamConstants.START_ELEMENT:
							writer.writeStartElement(prevName);
							for (int i=0; i < prevAtts.size(); i+=2) {
								writer.writeAttribute(prevAtts.get(i),
										prevAtts.get(i+1));
							}
							break;
						case XMLStreamConstants.END_ELEMENT:
							writer.writeEndElement();
							break;
						case XMLStreamConstants.CHARACTERS:
							writer.writeCharacters(prevText);
							break;
					}
				}
				// zapamiętujemy dane z aktualnie parsowanego obiektu
				prevEvent = event;
				prevName = parser.getLocalName();
				if (event == XMLStreamConstants.CHARACTERS) {
					prevText = parser.getText();
				}
				prevAtts.clear();
				if (event == XMLStreamConstants.START_ELEMENT) {
					for (int i=0; i < parser.getAttributeCount(); i++) {
						prevAtts.add(parser.getAttributeLocalName(i));
						prevAtts.add(parser.getAttributeValue(i));
					}
				}
			}
			writer.flush();
			writer.close();
			out.close();
			parser.close();
		} catch (XMLStreamException e) {
			System.out.println(e);
		} catch (IOException e) {
			System.out.println("IOException while parsing");
		}
	}
}
