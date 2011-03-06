package nibblr.http;

public class HttpRequestFactoryImpl implements HttpRequestFactory {

	@Override
	public HttpRequest createRequest(String url) {
		return new HttpRequestImpl(url);
	}
}
