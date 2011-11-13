package mdettla.jqsl;

public class SelectHaving extends AbstractSelect {

	SelectHaving(Column[] what, Table[] from, Predicate where,
			Column[] groupBy, Predicate having) {
		super();
		this.what = what;
		this.from = from;
		this.where = where;
		this.groupBy = groupBy;
		this.having = having;
	}

	public SelectOrderBy orderBy(Column... orderBy) {
		return new SelectOrderBy(what, from, where, groupBy, having, orderBy);
	}
}
