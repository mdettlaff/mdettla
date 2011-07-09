package mdettla.reddit.repository;

import java.util.Collection;

import mdettla.reddit.domain.Submission;

public interface SubmissionDao {

	void create(Submission submission);

	Submission findById(Long id);

	Collection<Submission> findAll();

	void update(Submission submission);

	void delete(Long id);
}
