package mdettla.reddit.service;

import mdettla.reddit.test.AbstractServiceTestContext;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;

public class AccountServiceTest extends AbstractServiceTestContext {

	@Autowired
	private AccountService accountService;

	@Test(expected = AccessDeniedException.class)
	public void testDisallowFindAllUsersByUser() {
		loginUser();
		accountService.findAllUsers();
	}

	@Test
	public void testAllowFindAllUsersByAdministrator() {
		loginAdministrator();
		accountService.findAllUsers();
	}

	@Test
	public void testAllowFindUserByNameByGuest() {
		accountService.findUserByName("foo");
	}
}
