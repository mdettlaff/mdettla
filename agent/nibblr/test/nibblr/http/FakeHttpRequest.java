package nibblr.http;

public class FakeHttpRequest implements HttpRequest {

	private String response;

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

	@Override
	public void setURL(String url) {
		if (url.contains("api.del.icio.us")) {
			response = DELICIOUS_RESPONSE;
		}
	}

	@Override
	public String doGet() {
		return response;
	}
}
