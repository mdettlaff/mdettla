package nibblr.domain;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

public class FeedTest {

	@Test
	public void gettersAndSetters() {
		Feed feed = getExampleFeed();

		assertEquals("delicious", feed.getName());
		assertEquals("http://delicious.com", feed.getURL());
		assertEquals(Arrays.asList("delicious", "links"), feed.getTags());
		List<FeedItem> items = feed.getItems();
		assertEquals(1, items.size());
		FeedItem firstItem = items.get(0);
		assertEquals("A cool page", firstItem.getTitle());
		assertEquals("http://coolpage.com", firstItem.getURL());
		assertEquals("the content", firstItem.getHTMLContent());
		assertEquals(FeedItemTest.NOW, firstItem.getDate());
	}

	private Feed getExampleFeed() {
		Feed feed = new Feed();
		feed.setName("delicious");
		feed.setURL("http://delicious.com");
		feed.addTags("delicious", "links");
		FeedItem item = FeedItemTest.getExampleFeedItem();
		feed.addItem(item);
		return feed;
	}
}
