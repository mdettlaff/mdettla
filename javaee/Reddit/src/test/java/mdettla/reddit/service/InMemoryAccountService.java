package mdettla.reddit.service;

import java.util.ArrayList;
import java.util.Collection;

import mdettla.reddit.domain.User;

public class InMemoryAccountService implements AccountService {

	@Override
	public Collection<User> findAllUsers() {
		Collection<User> users = new ArrayList<User>();
		users.add(new User("admin", "secret1"));
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
