package mdettla.reddit.service;

import static org.junit.Assert.assertEquals;
import mdettla.reddit.domain.Submission;
import mdettla.reddit.domain.User;
import mdettla.reddit.test.AbstractServiceTestContext;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.AuthenticationCredentialsNotFoundException;
import org.springframework.security.authentication.BadCredentialsException;

public class SubmissionServiceTest extends AbstractServiceTestContext {

	@Autowired
	private SubmissionService submissionService;
	private Submission submission;

	@Before
	public void setUp() {
		submission = new Submission();
		submission.setAuthor(new User("mdettla", "secret"));
		submission.setTitle("Example title");
		submission.upvote(new User("wundzun", "foobar"));
	}

	@Test(expected = AuthenticationCredentialsNotFoundException.class)
	public void testDisallowCreateByGuest() {
		submissionService.create(new Submission());
	}

	@Test(expected = BadCredentialsException.class)
	public void testDisallowCreateByUserWithWrongPassword() {
		attemptToLoginUserWithWrongPassword();
		submissionService.create(new Submission());
	}

	@Test(expected = BadCredentialsException.class)
	public void testDisallowCreateByUserWithWrongUsernameAndPassword() {
		attemptToLoginUserWithWrongUsernameAndPassword();
		submissionService.create(new Submission());
	}

	@Test
	public void testAllowCreateByUser() {
		loginUser();
		submissionService.create(new Submission());
	}

	@Test
	public void testAllowCreateByAdmin() {
		loginAdministrator();
		submissionService.create(new Submission());
	}

	@Test(expected = AuthenticationCredentialsNotFoundException.class)
	public void testDisallowUpdateByGuest() {
		submissionService.update(new Submission());
	}

	@Test
	public void testAllowUpdateByUser() {
		loginUser();
		submissionService.update(submission);
	}

	@Test(expected = AccessDeniedException.class)
	public void testDisallowUpdateByWrongUser() {
		loginUserWithNoSubmissionsCreated();
		submissionService.update(submission);
	}

	@Test
	public void testAllowUpdateByAdministrator() {
		loginAdministrator();
		submissionService.update(submission);
	}

	@Test(expected = AuthenticationCredentialsNotFoundException.class)
	public void testDisallowDeleteByGuest() {
		submissionService.delete(1L);
	}

	@Test(expected = AccessDeniedException.class)
	public void testDisallowDeleteByUser() {
		loginUser();
		submissionService.delete(1L);
	}

	@Test(expected = BadCredentialsException.class)
	public void testDisallowDeleteByAdministratorWithWrongPassword() {
		attemptToLoginAdministratorWithWrongPassword();
		submissionService.delete(1L);
	}

	@Test
	public void testAllowDeleteByAdministrator() {
		loginAdministrator();
		submissionService.delete(1L);
	}

	@Test
	public void testAllowFindAllByGuest() {
		submissionService.findAll();
	}

	@Test
	public void testAllowFindAllByAdministrator() {
		loginAdministrator();
		submissionService.findAll();
	}

	@Test
	public void testAllowFindByIdByGuest() {
		submissionService.findById(1L);
	}

	@Test(expected = AuthenticationCredentialsNotFoundException.class)
	public void testDisallowUpvoteByGuest() {
		submissionService.upvote(submission);
	}

	@Test
	public void testAllowUpvoteByUser() {
		loginUser();
		assertEquals(1, submission.getScore());
		submissionService.upvote(submission);
		assertEquals(2, submission.getScore());
	}

	@Test(expected = AccessDeniedException.class)
	public void testDisallowUpvoteByUserWhoAlreadyVoted() {
		loginUserWithNoSubmissionsCreated();
		assertEquals(1, submission.getScore());
		submissionService.upvote(submission);
	}
}
