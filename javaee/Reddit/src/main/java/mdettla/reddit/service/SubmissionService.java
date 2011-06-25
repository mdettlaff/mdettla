package mdettla.reddit.service;

import java.util.Collection;

import mdettla.reddit.dao.SubmissionDao;
import mdettla.reddit.domain.Submission;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SubmissionService {

	@Autowired
	private SubmissionDao dao;

	@Transactional
	public void create(Submission submission) {
		dao.create(submission);
	}

	@Transactional
	public Submission findById(Long id) {
		return dao.findById(id);
	}

	@Transactional
	public Collection<Submission> findAll() {
		return dao.findAll();
	}
}
