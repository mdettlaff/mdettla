package mdettla.reddit.web.controller;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.service.SubmissionService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
@RequestMapping("/submissions")
public class SubmissionController {

	private final SubmissionService submissionService;

	@Autowired
	public SubmissionController(SubmissionService submissionService) {
		this.submissionService = submissionService;
	}

	@RequestMapping(value = "/add", method = RequestMethod.GET)
	public void setupForm(Model model) {
		model.addAttribute(new Submission());
	}

	@RequestMapping(value = "/add", method = RequestMethod.POST)
	public String processSubmit(@ModelAttribute Submission submission) {
		submissionService.create(submission);
		return "redirect:/";
	}
}