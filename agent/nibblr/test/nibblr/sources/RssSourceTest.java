package nibblr.sources;

import static org.junit.Assert.assertEquals;

import java.util.List;

import nibblr.domain.FeedItem;
import nibblr.domain.FeedItemsSource;
import nibblr.http.FakeHttpRequestFactory;
import nibblr.http.HttpRequestFactory;

import org.junit.Test;

public class RssSourceTest {

	@Test
	public void getItemsFromRss() {
		String url = "http://www.joemonster.org/backend.php";
		HttpRequestFactory requestFactory = new FakeHttpRequestFactory();
		FeedItemsSource source = new RssSource(url, requestFactory);
		List<FeedItem> items = source.downloadItems();
		assertEquals(2, items.size());
		{
			FeedItem item = new FeedItem();
			item.setTitle("Autentyki CCCXCIII - Lekarskie pogaduchy");
			item.setUrl("http://www.joemonster.org/art/16474/Autentyki_CCCXCIII_Lekarskie_pogaduchy");
			// TODO other fields in item
			assertEquals(item, items.get(0));
		}
		{
			FeedItem item = new FeedItem();
			item.setTitle("Wielopak Weekendowy CDVI");
			item.setUrl("http://www.joemonster.org/art/16473/Wielopak_Weekendowy_CDVI");
			// TODO other fields in item
			assertEquals(item, items.get(1));
		}
	}
}
