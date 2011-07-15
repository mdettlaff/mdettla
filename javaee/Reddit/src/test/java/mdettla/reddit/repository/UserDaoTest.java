package mdettla.reddit.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Iterator;

import mdettla.reddit.domain.User;
import mdettla.reddit.test.AbstractPersistenceTestContext;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

public class UserDaoTest extends AbstractPersistenceTestContext {

	@Autowired
	private UserDao dao;

	@Test
	@Transactional
	public void testFindAllUsers() {
		// test
		Collection<User> allUsers = dao.findAllUsers();
		// verify
		assertNotNull(allUsers);
		assertEquals(2, allUsers.size());
		Iterator<User> usersIter = allUsers.iterator();
		User administrator = usersIter.next();
		assertEquals("administrator", administrator.getName());
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
		User user = dao.findUserByName("mdettla");
		assertEquals("mdettla", user.getName());
		assertEquals("secret", user.getPassword());
		assertFalse(user.isAdministrator());
	}

	@Test
	@Transactional
	public void testFindUserByNameWhenNotExists() {
		assertEquals(null, dao.findUserByName("nonexistent"));
	}

	@Test
	@Transactional
	public void testCreate() {
		// prepare
		Long id = 2L;
		User user = new User("johndoe", "foo");
		assertNull(dao.findUserByName("johndoe"));
		// test
		dao.create(user);
		// verify
		User actual = dao.findUserByName("johndoe");
		assertNotNull(actual);
		assertEquals(id, actual.getId());
		assertEquals("johndoe", actual.getName());
		assertEquals("foo", actual.getPassword());
		assertFalse(actual.isAdministrator());
	}
}
