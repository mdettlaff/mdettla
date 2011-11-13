package mdettla.jqsl;

public class SelectFrom extends AbstractSelect {

	SelectFrom(Column[] what, Table[] from) {
		super();
		this.what = what;
		this.from = from;
	}

	public SelectWhere where(Predicate where) {
		return new SelectWhere(what, from, where);
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
