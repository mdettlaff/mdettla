import javax.xml.stream.*;
import java.net.URL;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Map;
import java.util.HashMap;

public class TagCounter {

	public static void main(String[] args) {
		Map<String, Integer> tagsFrequencies = new HashMap<String, Integer>();

		if (args.length == 0) {
			System.err.println("Usage: java TagCounter file.xml");
			return;
		}
		String fileName = args[0];

		try {
			FileInputStream in = new FileInputStream(new File(fileName));
			XMLInputFactory factory = XMLInputFactory.newInstance();
			XMLStreamReader parser = factory.createXMLStreamReader(in);

			while (parser.hasNext()) {
				int event = parser.next();
				switch (event) {
					case XMLStreamConstants.START_ELEMENT:
						String name = parser.getLocalName();
						if (tagsFrequencies.containsKey(name)) {
							tagsFrequencies.put(name,
									tagsFrequencies.get(name) + 1);
						} else {
							tagsFrequencies.put(name, 1);
						}
						break;
				}
			}
			parser.close();
			for (Map.Entry entry : tagsFrequencies.entrySet()) {
				System.out.println(entry.getKey() + " " + entry.getValue());
			}
		} catch (XMLStreamException e) {
			System.out.println(e);
		} catch (IOException e) {
			System.out.println("IOException while parsing " + fileName);
		}
	}
}
