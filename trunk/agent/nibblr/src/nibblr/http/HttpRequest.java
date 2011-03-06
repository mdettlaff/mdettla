package nibblr.http;

public interface HttpRequest {

	void setURL(String url);

	String doGet();
}
