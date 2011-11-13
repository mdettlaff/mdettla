package mdettla.jqsl;

public class SelectGroupBy extends AbstractSelect {

	SelectGroupBy(Column[] what, Table[] from, Predicate where,
			Column[] groupBy, Predicate having) {
		this.what = what;
		this.from = from;
		this.where = where;
		this.groupBy = groupBy;
		this.having = having;
	}

	public SelectGroupBy orderBy(Column... orderBy) {
		this.orderBy = orderBy;
		return this;
	}
}
