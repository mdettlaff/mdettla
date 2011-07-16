package mdettla.reddit.test;

import org.junit.After;
import org.junit.runner.RunWith;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
public abstract class AbstractServiceTestContext {

	@After
	public void tearDown() {
		SecurityContextHolder.clearContext();
	}

	protected void loginUser() {
		loginUser("mdettla", "secret");
	}

	protected void loginUserWithNoSubmissionsCreated() {
		loginUser("wundzun", "foobar");
	}

	protected void attemptToLoginUserWithWrongPassword() {
		loginUser("mdettla", "bogus");
	}

	protected void attemptToLoginUserWithWrongUsernameAndPassword() {
		loginUser("nonexistent", "foo");
	}

	protected void loginAdministrator() {
		loginUser("administrator", "secret1");
	}

	protected void attemptToLoginAdministratorWithWrongPassword() {
		loginUser("administrator", "bogusbogus");
	}

	private void loginUser(String username, String password) {
		Authentication authentication =
			new UsernamePasswordAuthenticationToken(username, password);
		SecurityContextHolder.getContext().setAuthentication(authentication);
	}
}
