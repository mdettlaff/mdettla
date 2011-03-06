package nibblr.http;

public class FakeHttpRequestFactory implements HttpRequestFactory {

	public static final String DELICIOUS_RESPONSE =
		"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
		"<posts user=\"wundzun\" dt=\"2011-02-22T08:00:00Z\" tag=\"\">\n" +
		"  <post href=\"http://java.sun.com/developer/technicalArticles/Programming/Stacktrace/\" " +
  		"hash=\"997d794325c54a6c83539957f99eaf79\" " +
  		"description=\"An Introduction to Java Stack Traces\" " +
  		"tag=\"java debugging stack trace programming article\" " +
  		"time=\"2011-02-22T20:56:06Z\" " +
  		"extended=\"Useful advice on debugging Java programs.\"/>\n" +
  		"</posts>\n" +
  		"<!-- fe06.api.del.ac4.yahoo.net compressed/chunked Sun Mar  6 13:56:07 UTC 2011 -->";
	public static final String JOEMONSTER_RESPONSE =
		"<?xml version=\"1.0\" encoding=\"ISO-8859-2\"?>" +
		"<rss version=\"0.91\">" +
		"<channel>" +
		"<title>Joe Monster</title>" +
		"<link>http://www.joemonster.org</link>" +
		"<description>Joe Monster - Bo powaga zabija powoli...</description>" +
		"<language>pl</language>" +
		"<item>" +
		"<title>Autentyki CCCXCIII - Lekarskie pogaduchy</title>" +
		"<pubDate>Sun, 6 Mar 2011 05:00:01 +0100</pubDate>" +
		"<link>http://www.joemonster.org/art/16474/Autentyki_CCCXCIII_Lekarskie_pogaduchy</link>" +
		"<description><![CDATA[ Dziś o nerwowym kierowcy, szóstoklasiście, który sobie przemyśliwuje, oraz o najlepszym na świecie czujniku cofania.PRAWIDŁOWO PODSUMOWAŁSamochodem je... ]]></description>" +
		"</item>" +
		"<item>" +
		"<title>Wielopak Weekendowy CDVI</title>" +
		"<pubDate>Sat, 5 Mar 2011 05:00:01 +0100</pubDate>" +
		"<link>http://www.joemonster.org/art/16473/Wielopak_Weekendowy_CDVI</link>" +
		"<description><![CDATA[ W dzisiejszym wielopaku zaprosimy kogoś do mieszkania, porozmawiamy kulturalnie z policją i dowiemy się, po co komu potrzebna szynszyla...8 marca. Fac... ]]></description>" +
		"</item>\n" +
		"</channel>\n" +
		"</rss>";

	@Override
	public HttpRequest createRequest(String url) {
		if (url.contains("api.del.icio.us")) {
			return new FakeHttpRequest(DELICIOUS_RESPONSE);
		} else if (url.contains("joemonster.org")) {
			return new FakeHttpRequest(JOEMONSTER_RESPONSE);
		}
		throw new IllegalArgumentException("Unexpected URL: " + url);
	}
}
