import javax.xml.stream.*;
import java.net.URL;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

/**
 * Zadanie 2.
 * Napisac program, uzywajacy technologii StAX, ktory dla dokumentu z zadania 1
 * a) wypisze wszystkie elementy typu towar, dla ktorych VAT jest zerowy oraz
 * ilosc jest wieksza niz 7
 * b) wyliczy liczbe elementow spelniajacych powyzszy warunek.
 */
public class Towary {

	public static void main(String[] args) {

		boolean inTowar = false;
		boolean inIlosc = false;
		boolean inNazwa = false;
		String currentTowar = "";
		int iloscPasujacych = 0;
		double vat = .0;

		try {
			FileInputStream in = new FileInputStream(new File("faktura.xml"));
			XMLInputFactory factory = XMLInputFactory.newInstance();
			XMLStreamReader parser = factory.createXMLStreamReader(in);

			while (parser.hasNext()) {
				int event = parser.next();
				String name;
				switch (event) {
					case XMLStreamConstants.START_ELEMENT:
						name = parser.getLocalName();
						if ("towar".equals(name)) {
							inTowar = true;
							for (int i=0; i < parser.getAttributeCount(); i++) {
								if ("VAT".equals(parser.getAttributeName(i))) {
									String value = parser.getAttributeValue(i);
									vat = Double.parseDouble(value);
								}
							}
						}
						if ("ilosc".equals(name)) {
							inIlosc = true;
						}
						if ("nazwa".equals(name)) {
							inNazwa = true;
						}
						break;
					case XMLStreamConstants.END_ELEMENT:
						name = parser.getLocalName();
						if ("towar".equals(name)) {
							inTowar = false;
						}
						if ("ilosc".equals(name)) {
							inIlosc = false;
						}
						if ("nazwa".equals(name)) {
							inNazwa = false;
						}
						break;
					case XMLStreamConstants.CHARACTERS:
						String text = parser.getText();
						if (inTowar && inNazwa) {
							currentTowar = text;
						}
						if (inTowar && inIlosc) {
							int ilosc = Integer.parseInt(text);
							if (ilosc > 7 && vat == 0.0) {
								System.out.println(currentTowar);
								iloscPasujacych++;
							}
						}
				}
			}
			System.out.println("ilosc pasujacych elementow: "
					+ iloscPasujacych);
			parser.close();
		} catch (XMLStreamException e) {
			System.out.println(e);
		} catch (IOException e) {
			System.out.println("IOException while parsing");
		}
	}
}
