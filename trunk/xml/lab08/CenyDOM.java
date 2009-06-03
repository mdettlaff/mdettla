import java.io.FileOutputStream;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;

// rejestr do tworzenia implementacji DOM
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.bootstrap.DOMImplementationRegistry;

// Implementacja DOM Level 3 Load & Save
import org.w3c.dom.ls.DOMImplementationLS;
import org.w3c.dom.ls.LSParser; // Do serializacji (zapisywania) dokumentow
import org.w3c.dom.ls.LSSerializer;
import org.w3c.dom.ls.LSOutput;

// Konfigurator i obsluga bledow
import org.w3c.dom.DOMConfiguration;
import org.w3c.dom.DOMError;
import org.w3c.dom.DOMErrorHandler;

// Do pracy z dokumentem
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class CenyDOM {
	public static Document document,  document2;

	public static void main(String[] args) {
		if (args.length == 0) {
			printUsage();
			System.exit(1);
		}

		try {
			/*
			 * ustawienie systemowej wlasnosci (moze byc dokonane w innym
			 * miejscu, pliku konfiguracyjnym w systemie itp.) konkretna
			 * implementacja DOM
			 */
			System.setProperty(DOMImplementationRegistry.PROPERTY,
					"org.apache.xerces.dom.DOMXSImplementationSourceImpl");
			DOMImplementationRegistry registry = DOMImplementationRegistry.newInstance();

			// pozyskanie implementacji Load & Save DOM Level 3 z rejestru
			DOMImplementationLS impl =
				(DOMImplementationLS) registry.getDOMImplementation("LS");

			// stworzenie DOMBuilder
			LSParser builder = impl.createLSParser(
					DOMImplementationLS.MODE_SYNCHRONOUS, null);

			// pozyskanie konfiguratora - koniecznie zajrzec do dokumentacji co
			// mozna poustawiac
			DOMConfiguration config = builder.getDomConfig();

			// stworzenie DOMErrorHandler i zarejestrowanie w konfiguratorze
			DOMErrorHandler errorHandler = getErrorHandler();
			config.setParameter("error-handler", errorHandler);

			System.out.println("Parsowanie " + args[0] + "...");

			// sparsowanie dokumentu i pozyskanie "document" do dalszej pracy
			document = builder.parseURI(args[0]);

			DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
			DocumentBuilder db = dbf.newDocumentBuilder();
			document2 = db.newDocument();

			// praca z dokumentem, modyfikacja zawartosci etc...
			Map<String, List<Element>> standardy =
				new HashMap<String, List<Element>>();

			NodeList produkty = document.getElementsByTagName("produkt");
			for (int i = 0; i < produkty.getLength(); i++) {
				Element produkt = (Element) produkty.item(i);

				NodeList ceny = produkt.getElementsByTagName("cena");
				for (int x = 0; x < ceny.getLength(); x++) {
					Element cena = (Element) ceny.item(x);
					Element samochod = document2.createElement("samochod");

					String standard = cena.getAttribute("standard");
					samochod.setAttribute("marka",
							produkt.getAttribute("name"));
					samochod.setAttribute("cena", cena.getTextContent());
					if (!standardy.containsKey(standard)) {
						standardy.put(standard, new ArrayList<Element>());
					}
					standardy.get(standard).add(samochod);
				}
			}

			Element auta = (Element) document2.createElement("samochody");

			for (String standardNazwa : standardy.keySet()) {
				Element standard = document2.createElement("standard");
				standard.setAttribute("nazwa", standardNazwa);
				for (Element samochod : standardy.get(standardNazwa)) {
					standard.appendChild(samochod);
				}
				auta.appendChild(standard);
			}

			document2.appendChild(auta);

			// pozyskanie serializatora
			LSSerializer domWriter = impl.createLSSerializer();
			// pobranie konfiguratora dla serializatora
			config = domWriter.getDomConfig();
			config.setParameter("xml-declaration", Boolean.TRUE);
			config.setParameter("format-pretty-print", Boolean.TRUE);

			// pozyskanie i konfiguracja Wyjscia
			LSOutput dOut = impl.createLSOutput();
			//dOut.setEncoding("latin2");
			dOut.setByteStream(new FileOutputStream(args[1]));

			System.out.println("Serializing document... ");
			domWriter.write(document2, dOut);

			// Wyjscie na ekran
			//dOut.setByteStream(System.out);
			//domWriter.write(System.out, document2);

		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	private static void printUsage() {
		System.err.println("usage: java Ceny INPUT OUTPUT");
		System.err.println();
		System.err.println(
				"NOTE: You can only validate DOM tree against XML Schemas.");
	}

	// obsluga bledow za pomoca anonimowej klasy wewnetrznej implementujacej
	// DOMErrorHandler
	// por. SAX ErrorHandler

	public static DOMErrorHandler getErrorHandler() {
		return new DOMErrorHandler() {
			public boolean handleError(DOMError error) {
				short severity = error.getSeverity();
				if (severity == error.SEVERITY_ERROR)
					System.out.println("[dom3-error]: " + error.getMessage());
				if (severity == error.SEVERITY_WARNING)
					System.out.println("[dom3-warning]: " + error.getMessage());
				if (severity == error.SEVERITY_FATAL_ERROR)
					System.out.println("[dom3-fatal-error]: " + error.getMessage());
				return true;
			}
		};
	}
}
