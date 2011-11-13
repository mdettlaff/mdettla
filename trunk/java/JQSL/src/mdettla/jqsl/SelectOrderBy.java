package mdettla.jqsl;

public class SelectOrderBy extends AbstractSelect {

	SelectOrderBy(Column[] what, Table[] from, Predicate where,
			Column[] groupBy, Predicate having, Column[] orderBy) {
		this.what = what;
		this.from = from;
		this.where = where;
		this.groupBy = groupBy;
		this.having = having;
		this.orderBy = orderBy;
	}
}
