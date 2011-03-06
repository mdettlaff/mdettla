package nibblr.sources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.text.ParseException;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import nibblr.domain.FeedItem;
import nibblr.domain.FeedItemsSource;
import nibblr.http.FakeHttpRequest;

import org.junit.Test;

public class DeliciousTest {

	@Test
	public void parseDate() throws ParseException {
		String dateToParse = "2010-11-17T20:32:11Z";
		assertEquals(getExampleDate(), Delicious.parseDate(dateToParse));
	}

	@Test
	public void downloadOneFeedItem() {
		FeedItemsSource delicious = new Delicious(new FakeHttpRequest());
		List<FeedItem> items = delicious.downloadItems();
		assertFalse("No feed items found.", items.isEmpty());
		FeedItem actual = items.get(0);
		FeedItem expected = new FeedItem();
		expected.setURL("http://java.sun.com/developer/technicalArticles/Programming/Stacktrace/");
		expected.setTitle("An Introduction to Java Stack Traces");
		expected.setHTMLContent("Useful advice on debugging Java programs.");
		expected.setDate(getExampleDate());
		assertEquals(expected, actual);
	}

	private static Date getExampleDate() {
		Calendar calendar = Calendar.getInstance();
		calendar.set(Calendar.YEAR, 2010);
		calendar.set(Calendar.MONTH, 10);
		calendar.set(Calendar.DAY_OF_MONTH, 17);
		calendar.set(Calendar.HOUR_OF_DAY, 20);
		calendar.set(Calendar.MINUTE, 32);
		calendar.set(Calendar.SECOND, 11);
		calendar.set(Calendar.MILLISECOND, 0);
		return calendar.getTime();
	}
}
