package nibblr.http;

public interface HttpRequestFactory {

	HttpRequest createRequest(String url);
}
