package mdettla.reddit.domain;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToMany;

@Entity
public class Submission {

	@Id
	@GeneratedValue
	private Long id;
	private String title;
	private int upvoteCount;
	private int downvoteCount;
	@OneToMany(cascade = CascadeType.ALL)
	private List<Comment> comments;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public Submission() {
		comments = new ArrayList<Comment>();
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public void upvote() {
		upvoteCount++;
	}

	public void downvote() {
		downvoteCount++;
	}

	public int getScore() {
		return upvoteCount - downvoteCount;
	}

	public int getUpvoteCount() {
		return upvoteCount;
	}

	public void setUpvoteCount(int upvoteCount) {
		this.upvoteCount = upvoteCount;
	}

	public int getDownvoteCount() {
		return downvoteCount;
	}

	public void setDownvoteCount(int downvoteCount) {
		this.downvoteCount = downvoteCount;
	}

	public List<Comment> getComments() {
		return Collections.unmodifiableList(comments);
	}

	public void setComments(List<Comment> comments) {
		this.comments = comments;
	}

	public void addComment(Comment comment) {
		comments.add(comment);
	}

	@Override
	public String toString() {
		return "Submission[" +
			"title=" + title + ", upvoteCount=" + upvoteCount
			+ ", downvoteCount=" + downvoteCount +
			", comments=" + comments +
			"]";
	}
}
