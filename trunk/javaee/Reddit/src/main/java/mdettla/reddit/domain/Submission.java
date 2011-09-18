package mdettla.reddit.domain;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.validation.constraints.NotNull;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

@Entity
public class Submission {

	@Id
	@GeneratedValue
	private Long id;
	@NotNull
	private String title;
	@NotNull
	private int upvoteCount;
	@NotNull
	private int downvoteCount;
	@OneToMany(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
	private List<Comment> comments;
	@NotNull
	@ManyToOne(fetch = FetchType.EAGER)
	private User author;
	@ManyToMany
	private Set<User> voters;

	public Submission() {
		comments = new ArrayList<Comment>();
		voters = new LinkedHashSet<User>();
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public void upvote(User user) {
		upvoteCount++;
		voters.add(user);
	}

	public void downvote(User user) {
		downvoteCount++;
		voters.add(user);
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

	public User getAuthor() {
		return author;
	}

	public void setAuthor(User author) {
		this.author = author;
	}

	public Set<User> getVoters() {
		return Collections.unmodifiableSet(voters);
	}

	public void setVoters(Set<User> voters) {
		this.voters = voters;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Submission)) {
			return false;
		}
		Submission other = (Submission)obj;
		return new EqualsBuilder().append(id, other.id).build();
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(id).build();
	}

	@Override
	public String toString() {
		ToStringBuilder toStringBuilder = new ToStringBuilder(this);
		toStringBuilder.append(id);
		toStringBuilder.append(title);
		toStringBuilder.append(upvoteCount);
		toStringBuilder.append(downvoteCount);
		toStringBuilder.append(comments);
		return toStringBuilder.build();
	}
}
