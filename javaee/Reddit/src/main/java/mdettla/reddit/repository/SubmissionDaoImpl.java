package mdettla.reddit.repository;

import java.util.Collection;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.domain.Submission_;

import org.springframework.stereotype.Repository;

@Repository
public class SubmissionDaoImpl implements SubmissionDao {

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	public void create(Submission submission) {
		entityManager.persist(submission);
		entityManager.flush();
	}

	@Override
	public Submission findById(Long id) {
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		CriteriaQuery<Submission> query = criteriaBuilder.createQuery(Submission.class);
		Root<Submission> from = query.from(Submission.class);
		from.fetch(Submission_.voters, JoinType.LEFT);
		CriteriaQuery<Submission> select = query.select(from);
		select.where(criteriaBuilder.equal(from.get(Submission_.id), id));
		return QueryUtils.findEntity(entityManager.createQuery(select));
	}

	@Override
	public Collection<Submission> findAll() {
		CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
		CriteriaQuery<Submission> query = criteriaBuilder.createQuery(Submission.class);
		CriteriaQuery<Submission> select = query.select(query.from(Submission.class));
		return entityManager.createQuery(select).getResultList();
	}

	@Override
	public void update(Submission submission) {
		entityManager.merge(submission);
		entityManager.flush();
	}

	@Override
	public void delete(Long id) {
		Submission submission = findById(id);
		entityManager.remove(submission);
		entityManager.flush();
	}
}
