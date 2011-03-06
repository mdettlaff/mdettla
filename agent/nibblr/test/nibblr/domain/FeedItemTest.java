package nibblr.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.util.Date;

import org.junit.Test;

public class FeedItemTest {

	public static final Date NOW = new Date();

	@Test
	public void equality() {
		assertEquals(getExampleFeedItem(), getExampleFeedItem());
	}

	@Test
	public void inequality() {
		FeedItem differentItem = getExampleFeedItem();
		differentItem.setTitle("foo");
		assertFalse(getExampleFeedItem().equals(differentItem));
	}

	static FeedItem getExampleFeedItem() {
		FeedItem item = new FeedItem();
		item.setTitle("A cool page");
		item.setURL("http://coolpage.com");
		item.setHTMLContent("the content");
		item.setDate(NOW);
		return item;
	}
}
