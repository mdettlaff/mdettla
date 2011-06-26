package mdettla.reddit.service;

import java.util.Collection;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import mdettla.reddit.domain.Submission;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SubmissionServiceImpl implements SubmissionService {

	@PersistenceContext
	private EntityManager entityManager;

	@Override
	@Transactional
	public void create(Submission submission) {
		entityManager.persist(submission);
		entityManager.flush();
	}

	@Override
	@Transactional(readOnly = true)
	public Submission findById(Long id) {
		return entityManager.find(Submission.class, id);
	}

	@Override
	@Transactional(readOnly = true)
	@SuppressWarnings("unchecked")
	public Collection<Submission> findAll() {
		Query query = entityManager.createQuery("SELECT s FROM Submission s");
		return query.getResultList();
	}
}
