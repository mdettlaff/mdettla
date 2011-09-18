package mdettla.reddit.domain;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;

public class SubmissionTest {

	@Test
	public void testUpvote() {
		Submission submission = new Submission();
		assertEquals(0, submission.getScore());
		submission.upvote(new User());
		assertEquals(1, submission.getScore());
		assertEquals(1, submission.getVoters().size());
	}

	@Test
	public void testDownvote() {
		Submission submission = new Submission();
		assertEquals(0, submission.getScore());
		submission.downvote(new User());
		assertEquals(-1, submission.getScore());
		assertEquals(1, submission.getVoters().size());
	}

	@Test
	public void testUpvoteAndDownvote() {
		Submission submission = new Submission();
		assertEquals(0, submission.getScore());
		submission.downvote(new User("a", "b"));
		assertEquals(-1, submission.getScore());
		submission.upvote(new User("c", "d"));
		assertEquals(0, submission.getScore());
		submission.upvote(new User("e", "f"));
		assertEquals(1, submission.getScore());
		submission.upvote(new User("g", "h"));
		assertEquals(2, submission.getScore());
		submission.downvote(new User("i", "j"));
		assertEquals(1, submission.getScore());
		assertEquals(5, submission.getVoters().size());
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
