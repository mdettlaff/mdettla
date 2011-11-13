package mdettla.jqsl;

/**
 * Reprezentuje zapytanie lub część zapytania w języku SQL.
 */
abstract class SQL {

	protected final String query;

	protected SQL(String query) {
		this.query = query;
	}

	String getQuery() {
		return query;
	}

	@Override
	public String toString() {
		return getQuery();
	}
}
