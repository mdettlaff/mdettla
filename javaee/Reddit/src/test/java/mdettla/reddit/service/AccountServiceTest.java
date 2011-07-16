package mdettla.reddit.service;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import mdettla.reddit.domain.DuplicateUsernameException;
import mdettla.reddit.domain.User;
import mdettla.reddit.repository.UserDao;
import mdettla.reddit.test.AbstractServiceTestContext;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.AccessDeniedException;

public class AccountServiceTest extends AbstractServiceTestContext {

	@Autowired
	private AccountService accountService;
	@Autowired
	private UserDao userDao;

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

	@Test
	public void testAllowCreateUserByGuest() throws DuplicateUsernameException {
		User user = new User("johndoe", "foo");
		assertNull(userDao.findUserByName("johndoe"));
		accountService.createUser(user);
		assertSame(userDao.findUserByName("johndoe"), user);
	}
}
