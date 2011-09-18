package mdettla.reddit.web.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import mdettla.reddit.domain.Submission;
import mdettla.reddit.service.SubmissionService;

import org.junit.Before;
import org.junit.Test;
import org.springframework.ui.ExtendedModelMap;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.support.SessionStatus;
import org.springframework.web.servlet.ModelAndView;

public class SubmissionControllerTest {

	private SubmissionService submissionService;
	private SubmissionController controller;

	@Before
	public void setUp() {
		submissionService = mock(SubmissionService.class);
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
		// test
		String viewName = controller.submitAdd(submission);
		// verify
		assertEquals("redirect:/", viewName);
		verify(submissionService).create(submission);
	}

	@Test
	public void testDetails() {
		// prepare
		long id = 5;
		Submission submission = new Submission();
		submission.setId(id);
		// mock
		when(submissionService.findById(id)).thenReturn(submission);
		// test
		ModelAndView modelAndView = controller.details(id);
		// verify
		ModelMap model = modelAndView.getModelMap();
		String view = modelAndView.getViewName();
		assertEquals("submissions/details", view);
		Submission submissionInModel = (Submission)model.get("submission");
		assertSame(submission, submissionInModel);
	}

	@Test
	public void testSetupFormEdit() {
		// prepare
		long id = 5;
		Submission submission = new Submission();
		submission.setId(5L);
		// mock
		when(submissionService.findById(id)).thenReturn(submission);
		// test
		ModelAndView modelAndView = controller.setupFormEdit(id);
		// verify
		ModelMap model = modelAndView.getModelMap();
		String view = modelAndView.getViewName();
		assertEquals("submissions/edit", view);
		Submission submissionInModel = (Submission)model.get("submission");
		assertSame(submission, submissionInModel);
	}

	@Test
	public void testSubmitEdit() {
		// mock
		SessionStatus sessionStatus = mock(SessionStatus.class);
		// prepare
		Submission submission = new Submission();
		submission.setId(5L);
		// test
		String viewName = controller.submitEdit(submission, sessionStatus);
		// verify
		assertEquals("redirect:/", viewName);
		verify(submissionService).update(submission);
	}

	@Test
	public void testDelete() {
		long id = 5;
		// test
		String viewName = controller.delete(id);
		// verify
		assertEquals("redirect:/", viewName);
		verify(submissionService).delete(id);
	}

	@Test
	public void testUpvote() {
		// prepare
		long id = 5;
		Submission submission = new Submission();
		submission.setId(id);
		// mock
		when(submissionService.findById(id)).thenReturn(submission);
		// test
		String viewName = controller.upvote(id);
		// verify
		assertEquals("redirect:/submissions/" + id, viewName);
		verify(submissionService).upvote(submission);
	}

	@Test
	public void testDownvote() {
		// prepare
		long id = 5;
		Submission submission = new Submission();
		submission.setId(id);
		// mock
		when(submissionService.findById(id)).thenReturn(submission);
		// test
		String viewName = controller.downvote(id);
		// verify
		assertEquals("redirect:/submissions/" + id, viewName);
		verify(submissionService).downvote(submission);
	}
}
