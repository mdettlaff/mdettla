package mdettla.jqsl;

public class UnfinishedJoin extends SQL {

	UnfinishedJoin(String query) {
		super(query);
	}

	public Table on(Predicate predicate) {
		return new Table(getQuery() + " ON (" + predicate.getQuery() + ")");
	}

	public Table using(Column column) {
		return new Table(getQuery() + " USING (" + column.getQuery() + ")");
	}
}
