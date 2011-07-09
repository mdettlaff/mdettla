package mdettla.reddit.repository;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import mdettla.reddit.domain.Submission;

public class InMemorySubmissionDao implements SubmissionDao {

	private final Collection<Submission> submissions;

	public InMemorySubmissionDao() {
		submissions = new ArrayList<Submission>();
	}

	@Override
	public void create(Submission submission) {
		submissions.add(submission);
	}

	@Override
	public Submission findById(Long id) {
		for (Submission submission : submissions) {
			if (id != null && id.equals(submission.getId())) {
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
		if (existing != null) {
			existing.setTitle(submission.getTitle());
		}
	}

	@Override
	public void delete(Long id) {
		submissions.remove(findById(id));
	}
}
