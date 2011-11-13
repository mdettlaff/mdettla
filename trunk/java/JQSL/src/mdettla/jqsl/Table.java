package mdettla.jqsl;

public class Table extends SQL {

	Table(String query) {
		super(query);
	}

	public Table alias(String alias) {
		return new Table(getQuery() + ' ' + alias);
	}

	public UnfinishedJoin join(Table other) {
		return new UnfinishedJoin(
				getQuery() + " JOIN " + other.getQuery());
	}

	public UnfinishedJoin innerJoin(Table other) {
		return new UnfinishedJoin(
				getQuery() + " INNER JOIN " + other.getQuery());
	}

	public UnfinishedJoin crossJoin(Table other) {
		return new UnfinishedJoin(
				getQuery() + " CROSS JOIN " + other.getQuery());
	}

	public UnfinishedJoin leftJoin(Table other) {
		return new UnfinishedJoin(
				getQuery() + " LEFT JOIN " + other.getQuery());
	}

	public UnfinishedJoin leftOuterJoin(Table other) {
		return new UnfinishedJoin(
				getQuery() + " LEFT OUTER JOIN " + other.getQuery());
	}

	public UnfinishedJoin rightJoin(Table other) {
		return new UnfinishedJoin(
				getQuery() + " RIGHT JOIN " + other.getQuery());
	}

	public UnfinishedJoin rightOuterJoin(Table other) {
		return new UnfinishedJoin(
				getQuery() + " RIGHT OUTER JOIN " + other.getQuery());
	}

	public UnfinishedJoin fullJoin(Table other) {
		return new UnfinishedJoin(
				getQuery() + " FULL JOIN " + other.getQuery());
	}

	public UnfinishedJoin fullOuterJoin(Table other) {
		return new UnfinishedJoin(
				getQuery() + " FULL OUTER JOIN " + other.getQuery());
	}
}
