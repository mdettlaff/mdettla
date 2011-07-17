package mdettla.reddit.service;

import java.util.Collection;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.domain.User;
import mdettla.reddit.repository.SubmissionDao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class SubmissionServiceImpl implements SubmissionService {

	private final SubmissionDao submissionDao;
	private final AccountService accountService;

	@Autowired
	public SubmissionServiceImpl(SubmissionDao submissionDao, AccountService accountService) {
		this.submissionDao = submissionDao;
		this.accountService = accountService;
	}

	@Override
	@Transactional
	@PreAuthorize("isAuthenticated()")
	public void create(Submission submission) {
		submission.setAuthor(accountService.findCurrentUser());
		submissionDao.create(submission);
	}

	@Override
	@Transactional(readOnly = true)
	public Submission findById(Long id) {
		return submissionDao.findById(id);
	}

	@Override
	@Transactional(readOnly = true)
	public Collection<Submission> findAll() {
		return submissionDao.findAll();
	}

	@Override
	@Transactional
	@PreAuthorize("hasPermission(#submission, 'update')")
	public void update(Submission submission) {
		submissionDao.update(submission);
	}

	@Override
	@Transactional
	@PreAuthorize("hasRole('administrator')")
	public void delete(Long id) {
		submissionDao.delete(id);
	}

	@Override
	@Transactional
	@PreAuthorize("hasPermission(#submission, 'vote')")
	public void upvote(Submission submission) {
		User user = accountService.findCurrentUser();
		submission.upvote(user);
		submissionDao.update(submission);
	}
}
