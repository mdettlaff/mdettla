package mdettla.reddit.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.Collection;
import java.util.Iterator;

import mdettla.reddit.domain.Comment;
import mdettla.reddit.domain.Submission;
import mdettla.reddit.repository.SubmissionDao;
import mdettla.reddit.test.AbstractPersistenceTestContext;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;

public class SubmissionDaoTest extends AbstractPersistenceTestContext {

	@Autowired
	private SubmissionDao dao;

	@Test
	@Transactional
	public void testFindById() {
		// prepare
		Long id = 1L;
		// test
		Submission actual = dao.findById(id);
		// verify
		assertNotNull(actual);
		assertEquals(id, actual.getId());
		assertSubmissionsEqual(prepareSampleSubmission1(), actual);
	}

	@Test
	@Transactional
	public void testFindAll() {
		// test
		Collection<Submission> actual = dao.findAll();
		// verify
		assertNotNull(actual);
		assertEquals(2, actual.size());
		Iterator<Submission> submissions = actual.iterator();
		assertSubmissionsEqual(prepareSampleSubmission1(), submissions.next());
		assertSubmissionsEqual(prepareSampleSubmission2(), submissions.next());
	}

	@Test
	@Transactional
	public void testCreate() {
		// prepare
		Submission submission = new Submission();
		submission.setTitle("This is a picture of my dog");
		submission.setUpvoteCount(2);
		submission.setDownvoteCount(0);
		Comment comment = new Comment();
		comment.setContent("Cool dog");
		submission.addComment(comment);
		Long id = 3L;
		assertNull(dao.findById(id));
		// test
		dao.create(submission);
		// verify
		Submission actual = dao.findById(id);
		assertNotNull(actual);
		assertEquals(id, actual.getId());
		assertSubmissionsEqual(submission, actual);
	}

	@Test
	@Transactional
	public void testUpdate() {
		// prepare
		Submission submission = new Submission();
		submission.setId(1L);
		submission.setTitle("This is a picture of my kitty");
		submission.setUpvoteCount(3);
		submission.setDownvoteCount(1);
		// test
		dao.update(submission);
		// verify
		Submission updated = dao.findById(submission.getId());
		assertNotNull(updated);
		assertEquals(submission.getId(), updated.getId());
		assertEquals(submission.getTitle(), updated.getTitle());
		assertEquals(submission.getUpvoteCount(), updated.getUpvoteCount());
		assertEquals(submission.getDownvoteCount(), updated.getDownvoteCount());
	}

	@Test
	@Transactional
	public void testDelete() {
		// prepare
		Long id = 1L;
		// test
		dao.delete(id);
		// verify
		Submission deleted = dao.findById(id);
		assertNull(deleted);
	}

	private Submission prepareSampleSubmission1() {
		Submission submission = new Submission();
		submission.setTitle("This is a picture of my cat");
		submission.setUpvoteCount(3);
		submission.setDownvoteCount(1);
		Comment comment1 = new Comment();
		comment1.setContent("Cuteness overload");
		submission.addComment(comment1);
		Comment comment2 = new Comment();
		comment2.setContent("You're a kitty!");
		submission.addComment(comment2);
		return submission;
	}

	private Submission prepareSampleSubmission2() {
		Submission submission = new Submission();
		submission.setTitle("DAE breathe?");
		submission.setUpvoteCount(1);
		submission.setDownvoteCount(2);
		return submission;
	}

	private void assertSubmissionsEqual(Submission expected, Submission actual) {
		assertEquals(expected.getTitle(), actual.getTitle());
		assertEquals(expected.getDownvoteCount(), actual.getDownvoteCount());
		assertEquals(expected.getUpvoteCount(), actual.getUpvoteCount());
		assertEquals(expected.getScore(), actual.getScore());
		assertEquals(expected.getComments().size(), actual.getComments().size());
		for (int i = 0; i < expected.getComments().size(); i++) {
			Comment expectedComment = expected.getComments().get(i);
			Comment actualComment = actual.getComments().get(i);
			assertEquals(expectedComment.getContent(), actualComment.getContent());
		}
	}
}
