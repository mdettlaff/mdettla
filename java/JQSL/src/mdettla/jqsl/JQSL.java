package mdettla.jqsl;

public class JQSL {

	public static final Value VALUE = new Value();

	public static Select select(Column... columns) {
		return new Select(columns);
	}

	public static Column col(String columnName) throws JQSLException {
		return new Column(columnName, columnName, false);
	}

	public static Table tbl(String tableName) throws JQSLException {
		if (!tableName.matches("[0-9A-Za-z_]+(\\.[0-9A-Za-z_]+)?")) {
			throw new JQSLException("Invalid column name: " + tableName);
		}
		return new Table(tableName);
	}

	public static Column expr(String expression) throws JQSLException {
		return new Column(expression, false);
	}

	public static Column sum(Column column) throws JQSLException {
		return new Column("SUM(" + column.getQuery() + ")", true);
	}

	public static Column avg(Column column) throws JQSLException {
		return new Column("AVG(" + column.getQuery() + ")", true);
	}

	public static Column min(Column column) throws JQSLException {
		return new Column("MIN(" + column.getQuery() + ")", true);
	}

	public static Column max(Column column) throws JQSLException {
		return new Column("MAX(" + column.getQuery() + ")", true);
	}

	public static Column cast(Column column) throws JQSLException {
		return new Column("CAST(" + column.getQuery() + ")", false);
	}


	static String join(SQL... in) {
		StringBuilder out = new StringBuilder();
		if (in != null && in.length > 0) {
			for (int i = 0; i < in.length - 1; i++) {
				out.append(in[i].getQuery());
				out.append(", ");
			}
			out.append(in[in.length - 1]);
		}
		return out.toString();
	}
}
