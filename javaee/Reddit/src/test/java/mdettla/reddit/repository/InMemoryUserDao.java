package mdettla.reddit.repository;

import java.util.ArrayList;
import java.util.Collection;

import mdettla.reddit.domain.User;

public class InMemoryUserDao implements UserDao {

	@Override
	public Collection<User> findAllUsers() {
		Collection<User> users = new ArrayList<User>();
		users.add(new User("administrator", "secret1"));
		users.add(new User("mdettla", "secret"));
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
}
