package nibblr.sources;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import nibblr.domain.FeedItem;
import nibblr.domain.FeedItemsSource;
import nibblr.http.HttpRequest;
import nibblr.http.HttpRequestFactory;

import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.SyndFeedInput;

public class RssSource implements FeedItemsSource {

	private final HttpRequestFactory requestFactory;
	private final String url;

	public RssSource(String url, HttpRequestFactory requestFactory) {
		this.url = url;
		this.requestFactory = requestFactory;
	}

	@Override
	public List<FeedItem> downloadItems() {
		List<FeedItem> items;
		try {
			HttpRequest request = requestFactory.createRequest(url);
			String rss = request.doGet();
			items = parseRss(rss);
		} catch (IllegalArgumentException e) {
			throw new RuntimeException(e);
		} catch (FeedException e) {
			throw new RuntimeException(e);
		}
		return items;
	}

	private List<FeedItem> parseRss(String rss)
	throws IllegalArgumentException, FeedException {
		List<FeedItem> items = new ArrayList<FeedItem>();
		SyndFeedInput input = new SyndFeedInput();
		SyndFeed feed = input.build(new StringReader(rss));
		for (Object entry : feed.getEntries()) {
			SyndEntry rssItem = (SyndEntry)entry;
			FeedItem item = new FeedItem();
			item.setTitle(rssItem.getTitle());
			item.setUrl(rssItem.getUri());
			items.add(item);
		}
		return items;
	}
}
