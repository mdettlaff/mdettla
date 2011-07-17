package mdettla.reddit.domain;

import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.SetAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@StaticMetamodel(Submission.class)
public abstract class Submission_ {

	public static volatile SingularAttribute<Submission, Long> id;
	public static volatile SingularAttribute<Submission, User> author;
	public static volatile SingularAttribute<Submission, Integer> downvoteCount;
	public static volatile SingularAttribute<Submission, String> title;
	public static volatile SingularAttribute<Submission, Integer> upvoteCount;
	public static volatile ListAttribute<Submission, Comment> comments;
	public static volatile SetAttribute<Submission, User> voters;

}

