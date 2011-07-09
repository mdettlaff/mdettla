package mdettla.reddit.web.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import mdettla.reddit.domain.Submission;
import mdettla.reddit.repository.InMemorySubmissionDao;
import mdettla.reddit.service.SubmissionService;
import mdettla.reddit.service.SubmissionServiceImpl;

import org.junit.Before;
import org.junit.Test;
import org.springframework.ui.ExtendedModelMap;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.servlet.ModelAndView;

public class SubmissionControllerTest {

	private SubmissionService submissionService;
	private SubmissionController controller;

	@Before
	public void setUp() {
		submissionService = new SubmissionServiceImpl(new InMemorySubmissionDao());
		controller = new SubmissionController(submissionService);
	}

	@Test
	public void testSetupFormAdd() {
		// prepare
		Model model = new ExtendedModelMap();
		// test
		controller.setupFormAdd(model);
		// verify
		Submission submission = (Submission)model.asMap().get("submission");
		assertNotNull(submission);
	}

	@Test
	public void testSubmitAdd() {
		// prepare
		Submission submission = new Submission();
		submission.setTitle("foo");
		// test
		String viewName = controller.submitAdd(submission);
		// verify
		Submission expected = submissionService.findAll().iterator().next();
		assertNotNull(expected);
		assertEquals("foo", expected.getTitle());
		assertEquals("redirect:/", viewName);
	}

	@Test
	public void testDetails() {
		// prepare
		Submission submission = new Submission();
		submission.setId(5L);
		submission.setTitle("foo");
		submissionService.create(submission);
		// test
		ModelAndView modelAndView = controller.details(submission.getId());
		// verify
		ModelMap model = modelAndView.getModelMap();
		String view = modelAndView.getViewName();
		assertEquals("submissions/details", view);
		Submission submissionInModel = (Submission)model.get("submission");
		assertNotNull(submissionInModel);
		assertEquals(submissionInModel.getId(), submission.getId());
		assertEquals(submissionInModel.getTitle(), submission.getTitle());
	}

	@Test
	public void testSetupFormEdit() {
		// prepare
		Submission submission = new Submission();
		submission.setId(5L);
		submission.setTitle("foo");
		submissionService.create(submission);
		// test
		ModelAndView modelAndView = controller.setupFormEdit(submission.getId());
		// verify
		ModelMap model = modelAndView.getModelMap();
		String view = modelAndView.getViewName();
		assertEquals("submissions/edit", view);
		Submission submissionInModel = (Submission)model.get("submission");
		assertNotNull(submissionInModel);
		assertEquals(submissionInModel.getId(), submission.getId());
		assertEquals(submissionInModel.getTitle(), submission.getTitle());
	}

	@Test
	public void testSubmitEdit() {
		// prepare
		Submission originalSubmission = new Submission();
		originalSubmission.setId(5L);
		originalSubmission.setTitle("foo");
		submissionService.create(originalSubmission);
		Submission modifiedSubmission = new Submission();
		modifiedSubmission.setId(originalSubmission.getId());
		modifiedSubmission.setTitle("bar");
		// test
		String viewName = controller.submitEdit(modifiedSubmission);
		// verify
		assertEquals("redirect:/", viewName);
		Submission editedSubmission = submissionService.findById(5L);
		assertNotNull(editedSubmission);
		assertEquals(editedSubmission.getId(), originalSubmission.getId());
		assertEquals("bar", editedSubmission.getTitle());
	}

	@Test
	public void testDelete() {
		// prepare
		Submission submission = new Submission();
		submission.setId(5L);
		submissionService.create(submission);
		assertNotNull(submissionService.findById(submission.getId()));
		// test
		String viewName = controller.delete(submission.getId());
		// verify
		assertEquals("redirect:/", viewName);
		assertNull(submissionService.findById(submission.getId()));
	}
}
