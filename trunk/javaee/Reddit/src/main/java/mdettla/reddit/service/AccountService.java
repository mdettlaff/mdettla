package mdettla.reddit.service;

import java.util.Collection;

import mdettla.reddit.domain.User;

public interface AccountService {

	Collection<User> findAllUsers();

	User findUserByName(String username);
}
