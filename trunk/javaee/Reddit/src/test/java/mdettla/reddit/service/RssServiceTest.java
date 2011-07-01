package mdettla.reddit.service;

import static org.junit.Assert.assertEquals;
import mdettla.reddit.domain.Submission;

import org.junit.Before;
import org.junit.Test;

import com.sun.syndication.feed.rss.Channel;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.WireFeedOutput;

public class RssServiceTest {

	private SubmissionService submissionService;

	@Before
	public void setUp() {
		submissionService = new InMemorySubmissionService();
		Submission submission1 = new Submission();
		submission1.setId(1L);
		submission1.setTitle("foo");
		submissionService.create(submission1);
		Submission submission2 = new Submission();
		submission2.setId(2L);
		submission2.setTitle("bar");
		submissionService.create(submission2);
	}

	@Test
	public void testFindById() throws FeedException {
		// prepare
		RssService rssService = new RssService(submissionService);
		// test
		Channel channel = rssService.createRssChannel("http://foo.com/");
		// verify
		WireFeedOutput feedOutput = new WireFeedOutput();
		String actualContent = feedOutput.outputString(channel);
		String expectedContent =
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n" +
			"<rss version=\"2.0\">\r\n" +
			"  <channel>\r\n" +
			"    <title>Reddit</title>\r\n" +
			"    <link>http://foo.com/</link>\r\n" +
			"    <description>Reddit - voice of the Internet</description>\r\n" +
			"    <item>\r\n" +
			"      <title>foo</title>\r\n" +
			"      <link>http://foo.com/submissions/1</link>\r\n" +
			"    </item>\r\n" +
			"    <item>\r\n" +
			"      <title>bar</title>\r\n" +
			"      <link>http://foo.com/submissions/2</link>\r\n" +
			"    </item>\r\n" +
			"  </channel>\r\n" +
			"</rss>\r\n\r\n";
		assertEquals(expectedContent, actualContent);
	}
}
