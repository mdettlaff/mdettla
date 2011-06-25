package mdettla.reddit.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

public class SubmissionTest {

	@Test
	public void testUpvote() {
		Submission submission = new Submission();
		assertEquals(0, submission.getScore());
		submission.upvote();
		assertEquals(1, submission.getScore());
	}

	@Test
	public void testDownvote() {
		Submission submission = new Submission();
		assertEquals(0, submission.getScore());
		submission.downvote();
		assertEquals(-1, submission.getScore());
	}

	@Test
	public void testUpvoteAndDownvote() {
		Submission submission = new Submission();
		assertEquals(0, submission.getScore());
		submission.downvote();
		assertEquals(-1, submission.getScore());
		submission.upvote();
		assertEquals(0, submission.getScore());
		submission.upvote();
		assertEquals(1, submission.getScore());
		submission.upvote();
		assertEquals(2, submission.getScore());
		submission.downvote();
		assertEquals(1, submission.getScore());
	}

	@Test
	public void testComment() {
		Submission submission = new Submission();
		Comment comment = new Comment();
		comment.setContent("This is my comment");
		submission.addComment(comment);
		assertNotNull(submission.getComments());
		assertEquals(1, submission.getComments().size());
		Comment firstComment = submission.getComments().get(0);
		assertEquals("This is my comment", firstComment.getContent());
	}
}
