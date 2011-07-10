package mdettla.reddit.service;

import java.util.Collection;

import mdettla.reddit.domain.User;
import mdettla.reddit.repository.UserDao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AccountServiceImpl implements AccountService {

	private final UserDao userDao;

	@Autowired
	public AccountServiceImpl(UserDao userDao) {
		this.userDao = userDao;
	}

	@Override
	@Transactional(readOnly = true)
	@PreAuthorize("hasRole('administrator')")
	public Collection<User> findAllUsers() {
		return userDao.findAllUsers();
	}

	@Override
	@Transactional(readOnly = true)
	public User findUserByName(String username) {
		return userDao.findUserByName(username);
	}
}
