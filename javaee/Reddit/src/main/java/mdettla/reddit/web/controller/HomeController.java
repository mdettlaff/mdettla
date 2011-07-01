package mdettla.reddit.web.controller;

import java.util.Collection;

import javax.servlet.http.HttpServletRequest;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.service.RssService;
import mdettla.reddit.service.SubmissionService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.servlet.ModelAndView;

import com.sun.syndication.feed.rss.Channel;

@Controller
public class HomeController {

	private final SubmissionService submissionService;
	private final RssService rssService;

	@Autowired
	public HomeController(SubmissionService submissionService, RssService rssService) {
		this.submissionService = submissionService;
		this.rssService = rssService;
	}

	@RequestMapping(value = "/", method = RequestMethod.GET)
	public ModelAndView home() {
		Collection<Submission> submissions = submissionService.findAll();
		return new ModelAndView("index", "submissions", submissions);
	}

	@RequestMapping(value = "/rss", method = RequestMethod.GET)
	public @ResponseBody Channel rss(HttpServletRequest request) {
		String url = request.getRequestURL().toString();
		String link = url.substring(0, url.lastIndexOf('/') + 1);
		return rssService.createRssChannel(link);
	}
}
