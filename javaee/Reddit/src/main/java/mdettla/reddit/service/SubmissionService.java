package mdettla.reddit.service;

import java.util.Collection;

import mdettla.reddit.domain.Submission;

public interface SubmissionService {

	void create(Submission submission);

	Submission findById(Long id);

	Collection<Submission> findAll();

	void update(Submission submission);

	void delete(Long id);

	void upvote(Submission submission);

	void downvote(Submission submission);
}
