package mdettla.jqsl;

public class Predicate extends SQL {

	Predicate(String query) {
		super(query);
	}

	public Predicate and(Predicate other) {
		return new Predicate(getQuery() + " AND " + other.getQuery());
	}

	public Predicate or(Predicate other) {
		return new Predicate(
				"(" + getQuery() + " OR " + other.getQuery() + ")");
	}

	public Predicate not(Predicate other) {
		return new Predicate("NOT " + getQuery());
	}
}
