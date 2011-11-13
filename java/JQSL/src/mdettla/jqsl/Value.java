package mdettla.jqsl;

public class Value extends SQL {

	Value() {
		super("?");
	}

	public Column as(String alias) throws JQSLException {
		return new Column(getQuery() + " AS " + alias, alias, false);
	}
}
