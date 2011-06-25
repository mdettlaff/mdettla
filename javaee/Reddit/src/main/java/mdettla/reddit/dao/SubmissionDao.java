package mdettla.reddit.dao;

import java.util.Collection;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

import mdettla.reddit.domain.Submission;

import org.springframework.stereotype.Repository;

@Repository
public class SubmissionDao {

	@PersistenceContext
	private EntityManager em;

	public void create(Submission submission) {
		em.persist(submission);
		em.flush();
	}

	public Submission findById(Long id) {
		return em.find(Submission.class, id);
	}

	@SuppressWarnings("unchecked")
	public Collection<Submission> findAll() {
		Query query = em.createQuery("SELECT s FROM Submission s");
		return query.getResultList();
	}
}
