package mdettla.reddit.web.controller;

import java.util.Collection;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.service.SubmissionService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class HomeController {

	private final SubmissionService submissionService;

	@Autowired
	public HomeController(SubmissionService submissionService) {
		this.submissionService = submissionService;
	}

	@RequestMapping(value = "/")
	public ModelAndView home() {
		Collection<Submission> submissions = submissionService.findAll();
		return new ModelAndView("index", "submissions", submissions);
	}
}
