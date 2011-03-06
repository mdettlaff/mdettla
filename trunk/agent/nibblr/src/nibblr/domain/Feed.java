package nibblr.domain;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Feed {

	private String name;
	private String url;
	private List<String> tags;
	private List<FeedItem> items;

	public Feed() {
		tags = new ArrayList<String>();
		items = new ArrayList<FeedItem>();
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getURL() {
		return url;
	}

	public void setURL(String url) {
		this.url = url;
	}

	public void addTags(String... tags) {
		this.tags.addAll(Arrays.asList(tags));
	}

	public List<String> getTags() {
		return new ArrayList<String>(tags);
	}

	public void addItem(FeedItem item) {
		items.add(item);
	}

	public List<FeedItem> getItems() {
		return items;
	}
}
