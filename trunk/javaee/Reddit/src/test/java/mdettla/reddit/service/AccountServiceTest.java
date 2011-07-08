package mdettla.reddit.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Iterator;

import mdettla.reddit.domain.User;
import mdettla.reddit.test.AbstractPersistenceTestContext;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

public class AccountServiceTest extends AbstractPersistenceTestContext {

	@Autowired
	private AccountService service;

	@Test
	@Transactional
	public void testFindAllUsers() {
		// test
		Collection<User> allUsers = service.findAllUsers();
		// verify
		assertNotNull(allUsers);
		assertEquals(2, allUsers.size());
		Iterator<User> usersIter = allUsers.iterator();
		User administrator = usersIter.next();
		assertEquals("admin", administrator.getName());
		assertEquals("secret1", administrator.getPassword());
		assertTrue(administrator.isAdministrator());
		User user = usersIter.next();
		assertEquals("mdettla", user.getName());
		assertEquals("secret", user.getPassword());
		assertFalse(user.isAdministrator());
	}

	@Test
	@Transactional
	public void testFindUserByName() {
		User user = service.findUserByName("mdettla");
		assertEquals("mdettla", user.getName());
		assertEquals("secret", user.getPassword());
		assertFalse(user.isAdministrator());
	}

	@Test
	@Transactional
	public void testFindUserByNameWhenNotExists() {
		assertEquals(null, service.findUserByName("nonexistent"));
	}
}
