package mdettla.jqsl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

abstract public class AbstractSelect extends Table {

	protected Column[] what;
	protected Table[] from;
	protected Predicate where;
	protected Predicate having;
	protected Column[] groupBy;
	protected Column[] orderBy;

	protected AbstractSelect() {
		super(null);
	}

	public Table union(Table other) {
		return new Table(getQuery() + " UNION " + other.getQuery());
	}

	public Table unionAll(Table other) {
		return new Table(getQuery() + " UNION ALL " + other.getQuery());
	}

	private boolean isAnyAggregate() {
		boolean isAnyAggregate = false;
		for (Column column : what) {
			if (column.isAggregate()) {
				isAnyAggregate = true;
			}
		}
		return isAnyAggregate;
	}

	/**
	 * Automatycznie uzupełnia klauzulę GROUP BY o brakujące kolumny.
	 */
	private List<Column> completeGroupBy() {
		List<Column> completeGroupBy = new ArrayList<Column>();
		if (groupBy != null) {
			completeGroupBy.addAll(Arrays.asList(groupBy));
		}
		if (isAnyAggregate()) {
			for (Column column : what) {
				if (!column.isAggregate()
						&& !completeGroupBy.contains(column)) {
					completeGroupBy.add(column);
				}
			}
		}
		return completeGroupBy;
	}

	private String getInnerQuery() {
		StringBuilder select = new StringBuilder();
		select.append("SELECT ");
		select.append(JQSL.join(what));
		if (from != null) {
			select.append(" FROM ");
			select.append(JQSL.join(from));
		}
		if (where != null) {
			select.append(" WHERE ");
			select.append(JQSL.join(where));
		}
		if (!completeGroupBy().isEmpty()) {
			select.append(" GROUP BY ");
			select.append(JQSL.join(completeGroupBy().toArray(new Column[0])));
		}
		if (having != null) {
			select.append(" HAVING ");
			select.append(JQSL.join(having));
		}
		if (orderBy != null) {
			select.append(" ORDER BY ");
			select.append(JQSL.join(orderBy));
		}
		return select.toString();
	}

	@Override
	String getQuery() {
		return "(" + getInnerQuery() + ")";
	}

	@Override
	public String toString() {
		return getInnerQuery();
	}
}
