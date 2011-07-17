package mdettla.reddit.repository;

import java.util.Collection;

import javax.persistence.TypedQuery;

public class QueryUtils {

	public static <T> T findEntity(TypedQuery<T> query) {
		Collection<T> results = query.getResultList();
		return results.isEmpty() ? null : results.iterator().next();
	}
}
