package mdettla.reddit.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import mdettla.reddit.domain.Submission;

public class InMemorySubmissionService implements SubmissionService {

	private final Collection<Submission> submissions;

	public InMemorySubmissionService() {
		submissions = new ArrayList<Submission>();
	}

	@Override
	public void create(Submission submission) {
		submissions.add(submission);
	}

	@Override
	public Submission findById(Long id) {
		for (Submission submission : submissions) {
			if (id.equals(submission.getId())) {
				return submission;
			}
		}
		return null;
	}

	@Override
	public Collection<Submission> findAll() {
		return Collections.unmodifiableCollection(submissions);
	}

	@Override
	public void update(Submission submission) {
		Submission existing = findById(submission.getId());
		existing.setTitle(submission.getTitle());
	}

	@Override
	public void delete(Long id) {
		submissions.remove(findById(id));
	}
}
