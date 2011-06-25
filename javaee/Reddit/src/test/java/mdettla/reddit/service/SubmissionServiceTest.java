package mdettla.reddit.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.Collection;
import java.util.Iterator;

import mdettla.reddit.domain.Comment;
import mdettla.reddit.domain.Submission;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
public class SubmissionServiceTest {

	@Autowired
	private SubmissionService service;

	@Test
	public void testFindById() {
		// prepare
		Long id = 1L;
		// test
		Submission actual = service.findById(id);
		// verify
		assertNotNull(actual);
		assertEquals(id, actual.getId());
		assertSubmissionsEqual(prepareSampleSubmission1(), actual);
	}

	@Test
	public void testFindAll() {
		// test
		Collection<Submission> actual = service.findAll();
		// verify
		assertNotNull(actual);
		assertEquals(2, actual.size());
		Iterator<Submission> submissions = actual.iterator();
		assertSubmissionsEqual(prepareSampleSubmission1(), submissions.next());
		assertSubmissionsEqual(prepareSampleSubmission2(), submissions.next());
	}

	@Test
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
		assertNull(service.findById(id));
		// test
		service.create(submission);
		// verify
		Submission actual = service.findById(id);
		assertNotNull(actual);
		assertEquals(id, actual.getId());
		assertSubmissionsEqual(submission, actual);
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
