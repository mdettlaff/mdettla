package mdettla.jqsl;

import static mdettla.jqsl.JQSL.col;
import static mdettla.jqsl.JQSL.select;
import static mdettla.jqsl.JQSL.tbl;

import mdettla.jqsl.JQSLException;

import org.junit.Test;


public class JQSLErrorsTest {

	@Test(expected=JQSLException.class)
	public void testInvalidColumnName() throws JQSLException {
		select(col("FOO BAR")).from(tbl("BAZ"));
	}

	@Test(expected=JQSLException.class)
	public void testInvalidTableName() throws JQSLException {
		select(col("FOO")).from(tbl("BAR BAZ"));
	}
}
