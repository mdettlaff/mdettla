package mdettla.reddit.web.controller;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;

import javax.servlet.http.HttpServletRequest;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.service.RssService;
import mdettla.reddit.service.SubmissionService;

import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.web.servlet.ModelAndView;

import com.sun.syndication.feed.rss.Channel;

public class HomeControllerTest {

	private HomeController controller;

	@Before
	public void setUp() {
		SubmissionService submissionService = mock(SubmissionService.class);
		when(submissionService.findAll()).thenReturn(
				Arrays.asList(new Submission(), new Submission(), new Submission()));
		RssService rssService = new RssService(submissionService);
		controller = new HomeController(submissionService, rssService);
	}

	@Test
	public void testHome() {
		// test
		ModelAndView modelAndView = controller.home();
		// verify
		@SuppressWarnings("unchecked")
		Collection<Submission> submissions =
			(Collection<Submission>)modelAndView.getModel().get("submissions");
		assertEquals(3, submissions.size());
		assertEquals("index", modelAndView.getViewName());
	}

	@Test
	public void testRss() {
		HttpServletRequest request = new MockHttpServletRequest("GET", "/reddit/rss");
		// test
		Channel channel = controller.rss(request);
		// verify
		assertEquals("http://localhost:80/reddit/", channel.getLink());
		assertEquals(3, channel.getItems().size());
	}
}
