package mdettla.reddit.repository;

import java.util.Collection;

import mdettla.reddit.domain.User;

public interface UserDao {

	Collection<User> findAllUsers();

	User findUserByName(String username);

	void create(User user);
}
