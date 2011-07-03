package mdettla.reddit.service;

import mdettla.reddit.domain.Submission;

import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
public class SubmissionServiceSecurityTest {

	@Autowired
	private SubmissionService submissionService;

	@After
	public void tearDown() {
		SecurityContextHolder.clearContext();
	}

	@Test(expected = BadCredentialsException.class)
	public void testDisallowUpdate() {
		loginUser("mdettla", "bogus");
		submissionService.update(new Submission());
	}

	@Test
	public void testAllowUpdate() {
		loginUser("mdettla", "secret");
		submissionService.update(new Submission());
	}

	@Test(expected = BadCredentialsException.class)
	public void testDisallowDelete() {
		loginUser("mdettla", "bogus");
		submissionService.delete(1L);
	}

	@Test
	public void testAllowDelete() {
		loginUser("mdettla", "secret");
		submissionService.delete(1L);
	}

	@Test(expected = BadCredentialsException.class)
	public void testDisallowCreate() {
		loginUser("mdettla", "bogus");
		submissionService.create(new Submission());
	}

	@Test
	public void testAllowCreate() {
		loginUser("mdettla", "secret");
		submissionService.create(new Submission());
	}

	@Test
	public void testAllowFindAll() {
		loginUser("mdettla", "bogus");
		submissionService.findAll();
	}

	@Test
	public void testAllowFindById() {
		loginUser("mdettla", "bogus");
		submissionService.findById(1L);
	}

	private void loginUser(String username, String password) {
		Authentication authentication =
			new UsernamePasswordAuthenticationToken(username, password);
		SecurityContextHolder.getContext().setAuthentication(authentication);
	}
}
