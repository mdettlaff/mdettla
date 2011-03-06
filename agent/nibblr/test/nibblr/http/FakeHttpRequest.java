package nibblr.http;

class FakeHttpRequest implements HttpRequest {

	private final String response;

	public FakeHttpRequest(String response) {
		this.response = response;
	}

	@Override
	public String doGet() {
		return response;
	}
}
