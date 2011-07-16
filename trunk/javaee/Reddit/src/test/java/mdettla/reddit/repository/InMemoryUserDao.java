package mdettla.reddit.repository;

import java.util.ArrayList;
import java.util.Collection;

import mdettla.reddit.domain.User;

public class InMemoryUserDao implements UserDao {

	private final Collection<User> users;

	public InMemoryUserDao() {
		users = new ArrayList<User>();
		users.add(new User("administrator", "secret1"));
		users.add(new User("mdettla", "secret"));
		users.add(new User("wundzun", "foobar"));
	}

	@Override
	public Collection<User> findAllUsers() {
		return users;
	}

	@Override
	public User findUserByName(String username) {
		for (User user : findAllUsers()) {
			if (username.equals(user.getName())) {
				return user;
			}
		}
		return null;
	}

	@Override
	public void create(User user) {
		users.add(user);
	}
}
