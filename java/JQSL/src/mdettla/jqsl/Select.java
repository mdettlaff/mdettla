package mdettla.jqsl;

public class Select extends AbstractSelect {

	Select(Column... what) {
		super();
		this.what = what;
	}

	public SelectFrom from(Table... from) throws JQSLException {
		return new SelectFrom(what, from);
	}
}
