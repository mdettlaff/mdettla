package nibblr.http;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class HttpRequestIntegrationTest {

	// this test is turned off by default, because it requires
	// an Internet connection
	@org.junit.Ignore
	@Test
	public void simpleGET() {
		HttpRequest request = new HttpRequestImpl();
		request.setURL("http://www.google.com/search?hl=pl");
		String response = request.doGet();
		assertTrue("Unexpected HTTP response.", isGooglePage(response));
	}

	private static boolean isGooglePage(String html) {
		return html.contains("value=\"Szukaj w Google\"");
	}
}
