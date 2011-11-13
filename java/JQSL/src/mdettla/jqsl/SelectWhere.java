package mdettla.jqsl;

public class SelectWhere extends AbstractSelect {

	SelectWhere(Column[] what, Table[] from, Predicate where) {
		super();
		this.what = what;
		this.from = from;
		this.where = where;
	}

	public SelectHaving having(Predicate having) {
		return new SelectHaving(what, from, where, groupBy, having);
	}

	public SelectGroupBy groupBy(Column... groupBy) {
		return new SelectGroupBy(what, from, where, groupBy, having);
	}

	public SelectOrderBy orderBy(Column... orderBy) {
		return new SelectOrderBy(what, from, where, groupBy, having, orderBy);
	}
}
