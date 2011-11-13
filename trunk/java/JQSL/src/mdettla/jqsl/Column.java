package mdettla.jqsl;

public class Column extends SQL {

	private final String name;
	private final boolean isAggregate;

	Column(String query, boolean isAggregate) {
		super(query);
		this.isAggregate = isAggregate;
		this.name = null;
	}

	Column(String query, String name, boolean isAggregate)
		throws JQSLException {
		super(query);
		if (!name.matches("[0-9A-Za-z_]+(\\.[0-9A-Za-z_]+)?")) {
			throw new JQSLException("Invalid column name: " + name);
		}
		this.name = name;
		this.isAggregate = isAggregate;
	}

	public Column as(String alias) throws JQSLException {
		return new Column(getQuery() + " AS " + alias, alias, isAggregate);
	}

	public Column asc() throws JQSLException {
		return new Column(getQuery() + " ASC", false);
	}

	public Column desc() throws JQSLException {
		return new Column(getQuery() + " DESC", false);
	}

	public Predicate eq(Column other) {
		return new Predicate(getQuery() + " = " + other.getQuery());
	}

	public Predicate eq(Value value) {
		return new Predicate(getQuery() + " = " + value);
	}

	public Predicate uneq(Column other) {
		return new Predicate(getQuery() + " <> " + other.getQuery());
	}

	public Predicate uneq(Value value) {
		return new Predicate(getQuery() + " <> " + value);
	}

	public Predicate gt(Column other) {
		return new Predicate(getQuery() + " > " + other.getQuery());
	}

	public Predicate gt(Value value) {
		return new Predicate(getQuery() + " > " + value);
	}

	public Predicate lt(Column other) {
		return new Predicate(getQuery() + " < " + other.getQuery());
	}

	public Predicate lt(Value value) {
		return new Predicate(getQuery() + " < " + value);
	}

	public Predicate ge(Column other) {
		return new Predicate(getQuery() + " >= " + other.getQuery());
	}

	public Predicate ge(Value value) {
		return new Predicate(getQuery() + " >= " + value);
	}

	public Predicate le(Column other) {
		return new Predicate(getQuery() + " =< " + other.getQuery());
	}

	public Predicate le(Value value) {
		return new Predicate(getQuery() + " =< " + value);
	}

	boolean isAggregate() {
		return isAggregate;
	}

	@Override
	public boolean equals(Object other) {
		if (!(other instanceof Column)) {
			return false;
		}
		return name.equals(((Column)other).name);
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}
}
