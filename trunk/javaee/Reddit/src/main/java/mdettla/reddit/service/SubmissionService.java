package mdettla.reddit.service;

import java.util.Collection;

import mdettla.reddit.domain.Submission;

public interface SubmissionService {

	abstract void create(Submission submission);

	abstract Submission findById(Long id);

	abstract Collection<Submission> findAll();

	abstract void update(Submission submission);

	abstract void delete(Long id);
}
