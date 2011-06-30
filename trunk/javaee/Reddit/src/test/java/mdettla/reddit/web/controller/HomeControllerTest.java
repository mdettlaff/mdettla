package mdettla.reddit.web.controller;

import static org.junit.Assert.assertEquals;

import java.util.Collection;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.service.InMemorySubmissionService;
import mdettla.reddit.service.RssService;
import mdettla.reddit.service.SubmissionService;

import org.junit.Before;
import org.junit.Test;
import org.springframework.web.servlet.ModelAndView;

import com.sun.syndication.feed.rss.Channel;

public class HomeControllerTest {

	private HomeController controller;

	@Before
	public void setUp() {
		SubmissionService submissionService = new InMemorySubmissionService();
		submissionService.create(new Submission());
		submissionService.create(new Submission());
		submissionService.create(new Submission());
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
		// test
		Channel channel = controller.rss();
		// verify
		assertEquals(3, channel.getItems().size());
	}
}
