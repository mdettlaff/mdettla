package mdettla.reddit.service;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import mdettla.reddit.domain.Submission;

import org.junit.Test;

import com.sun.syndication.feed.rss.Channel;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.WireFeedOutput;

public class RssServiceTest {

	@Test
	public void testCreateRssChannel() throws FeedException {
		// mock
		SubmissionService submissionService = mock(SubmissionService.class);
		when(submissionService.findAll()).thenReturn(prepareSubmissions());
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

	private List<Submission> prepareSubmissions() {
		List<Submission> submissions = new ArrayList<Submission>();
		Submission submission1 = new Submission();
		submission1.setId(1L);
		submission1.setTitle("foo");
		submissions.add(submission1);
		Submission submission2 = new Submission();
		submission2.setId(2L);
		submission2.setTitle("bar");
		submissions.add(submission2);
		return submissions;
	}
}
