package mdettla.reddit.service;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.test.AbstractServiceTestContext;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.authentication.AuthenticationCredentialsNotFoundException;
import org.springframework.security.authentication.BadCredentialsException;

public class SubmissionServiceTest extends AbstractServiceTestContext {

	@Autowired
	private SubmissionService submissionService;

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
		submissionService.update(new Submission());
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
}
