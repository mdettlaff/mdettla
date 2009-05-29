import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.ArrayList;

import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.filter.ElementFilter;
import org.jdom.input.SAXBuilder;

public class PrintTag {

	public static void main(String[] args) {
		if (args.length < 3) {
			System.out.println("Usage: java PrintTag URI TAG INDEX");
			System.exit(2);
		}

		SAXBuilder builder = new SAXBuilder();

		try {
			Document document = builder.build(args[0]);
			ElementFilter elementFilter = new ElementFilter(args[1]);
			int tagIndex = Integer.parseInt(args[2]);
			List<Element> matchingElements = new ArrayList<Element>();
			Iterator it = document.getDescendants(elementFilter);
			while (it.hasNext()) {
				Element element = (Element) it.next();
				matchingElements.add(element);
			}
			if (matchingElements.size() == 0) {
				System.out.println("Brak elementów o podanej nazwie.");
				System.exit(0);
			}
			tagIndex = tagIndex < matchingElements.size() ? tagIndex
				: matchingElements.size() - 1;
			Element element = matchingElements.get(tagIndex);
			System.out.println("Element: " + element.getName());
			System.out.println("Zawartość: " + element.getTextTrim());
			System.out.println("Atrybuty:");
			for (Object obj : element.getAttributes()) {
				Attribute attribute = (Attribute) obj;
				System.out.println("    " + attribute.getName()
						+ " = " + attribute.getValue());
			}
		} catch (JDOMException e) {
			System.out.println(args[0] + " nie jest poprawny składniowo.");
			System.out.println(e.getMessage());
		} catch (IOException e) {
			System.out.println("Nie można odczytać " + args[0]);
		}
	}
}
