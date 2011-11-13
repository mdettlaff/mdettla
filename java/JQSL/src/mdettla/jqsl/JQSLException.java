package mdettla.jqsl;

import java.sql.SQLException;

public class JQSLException extends SQLException {

	JQSLException(String message) {
		super(message);
	}
}
