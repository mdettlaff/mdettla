package mdettla.reddit.service;

import java.util.Collection;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import mdettla.reddit.domain.User;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class AccountServiceImpl implements AccountService {

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	@Transactional(readOnly = true)
	@SuppressWarnings("unchecked")
	public Collection<User> findAllUsers() {
		Query query = entityManager.createQuery("SELECT u FROM User u");
		return query.getResultList();
	}

	@Override
	public User findUserByName(String username) {
		Query query = entityManager.createQuery(
				"SELECT u FROM User u WHERE name = :name");
		query.setParameter("name", username);
		@SuppressWarnings("unchecked")
		Collection<User> results = query.getResultList();
		if (results.isEmpty()) {
			return null;
		} else {
			return results.iterator().next();
		}
	}
}
