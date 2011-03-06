package nibblr.sources;

import java.io.Reader;
import java.io.StringReader;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import nibblr.domain.FeedItem;
import nibblr.domain.FeedItemsSource;
import nibblr.http.HttpRequest;

public class Delicious implements FeedItemsSource {

	private final HttpRequest request;

	public Delicious(HttpRequest request) {
		this.request = request;
	}

	@Override
	public List<FeedItem> downloadItems() {
		List<FeedItem> items;
		try {
			String xmlResponse = getXMLResponse();
			items = parseXMLResponse(xmlResponse);
		} catch (XMLStreamException e) {
			throw new RuntimeException(e);
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
		return items;
	}

	private String getXMLResponse() {
		request.setURL("https://api.del.icio.us/v1/posts/get");
		String xmlResponse = request.doGet();
		return xmlResponse;
	}

	private List<FeedItem> parseXMLResponse(String xmlResponse)
	throws XMLStreamException, ParseException {
		Reader reader = new StringReader(xmlResponse);
		List<FeedItem> items = new ArrayList<FeedItem>();
		XMLInputFactory inputFactory = XMLInputFactory.newInstance();
		XMLStreamReader xmlReader = inputFactory.createXMLStreamReader(reader);
		while (xmlReader.hasNext()) {
			switch (xmlReader.next()) {
			case XMLStreamConstants.START_ELEMENT:
				if ("post".equals(xmlReader.getLocalName())) {
					FeedItem item = parseDeliciousPost(xmlReader);
					items.add(item);
				}
			}
		}
		return items;
	}

	private FeedItem parseDeliciousPost(XMLStreamReader xmlReader)
	throws ParseException {
		FeedItem item = new FeedItem();
		item.setURL(xmlReader.getAttributeValue(null, "href"));
		item.setTitle(xmlReader.getAttributeValue(null, "description"));
		item.setHTMLContent(xmlReader.getAttributeValue(null, "extended"));
		String date = xmlReader.getAttributeValue(null, "time");
		item.setDate(parseDate(date));
		return item;
	}

	static Date parseDate(String dateToParse) throws ParseException {
		final String FORMAT = "yyyy-MM-dd'T'HH:mm:ss";
		DateFormat dateFormat = new SimpleDateFormat(FORMAT);
		return dateFormat.parse(dateToParse);
	}
}
