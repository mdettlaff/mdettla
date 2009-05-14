import javax.xml.stream.*;
import java.net.URL;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public class TagPath {

	public static void main(String[] args) {
		LinkedList<String> path = new LinkedList<String>();
		List<String> tags = new LinkedList<String>();

		if (args.length < 2) {
			System.err.println("Usage: java TagPath FILE TAG...");
			return;
		}
		String fileName = args[0];
		for (int i=1; i < args.length; i++) {
			tags.add(args[i]);
		}	

		try {
			FileInputStream in = new FileInputStream(new File(fileName));
			XMLInputFactory factory = XMLInputFactory.newInstance();
			XMLStreamReader parser = factory.createXMLStreamReader(in);

			while (parser.hasNext()) {
				int event = parser.next();
				switch (event) {
					case XMLStreamConstants.START_ELEMENT:
						String name = parser.getLocalName();
						path.addLast(name);
						if (tags.contains(name)) {
							System.out.println(path);
						}
						break;
					case XMLStreamConstants.END_ELEMENT:
						path.removeLast();
						break;
				}
			}
			parser.close();
		} catch (XMLStreamException e) {
			System.out.println(e);
		} catch (IOException e) {
			System.out.println("IOException while parsing " + fileName);
		}
	}
}
