package mdettla.reddit.web.controller;

import java.util.Collection;
import java.util.Date;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.service.SubmissionService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class HomeController {

	@Autowired
	private SubmissionService submissionService;

	@RequestMapping(value = "/")
	public ModelAndView home() {
		System.out.println("HomeController: home() begin");
		Submission submission = new Submission();
		submission.setTitle("News at " + new Date());
		submissionService.create(submission);
		Collection<Submission> submissions = submissionService.findAll();
		return new ModelAndView("index", "submissions", submissions);
	}
}
