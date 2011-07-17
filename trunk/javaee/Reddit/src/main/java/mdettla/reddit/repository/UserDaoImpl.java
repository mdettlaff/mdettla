package mdettla.reddit.repository;

import java.util.Collection;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import mdettla.reddit.domain.User;
import mdettla.reddit.domain.User_;

import org.springframework.stereotype.Repository;

@Repository
public class UserDaoImpl implements UserDao {

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public Collection<User> findAllUsers() {
		CriteriaQuery<User> query = entityManager.getCriteriaBuilder().createQuery(User.class);
		return entityManager.createQuery(query.select(query.from(User.class))).getResultList();
	}

	@Override
	public User findUserByName(String username) {
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		CriteriaQuery<User> query = criteriaBuilder.createQuery(User.class);
		Root<User> from = query.from(User.class);
		query.where(criteriaBuilder.equal(from.get(User_.name), username));
		return QueryUtils.findEntity(entityManager.createQuery(query.select(from)));
	}
	@Override
	public void create(User user) {
		entityManager.persist(user);
		entityManager.flush();
	}
}
