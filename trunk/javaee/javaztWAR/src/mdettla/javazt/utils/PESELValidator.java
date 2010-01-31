package mdettla.javazt.utils;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.context.FacesContext;
import javax.faces.validator.Validator;
import javax.faces.validator.ValidatorException;

public class PESELValidator implements Validator {

	@Override
	public void validate(FacesContext context, UIComponent component, Object value)
			throws ValidatorException {
		boolean validated = false;
		String p = (String) value;
		if (p.length() == 11) {
			int sum = 0;
			sum += 1*Integer.parseInt(p.substring(0,1));
			sum += 3*Integer.parseInt(p.substring(1,2));
			sum += 7*Integer.parseInt(p.substring(2,3));
			sum += 9*Integer.parseInt(p.substring(3,4));
			sum += 1*Integer.parseInt(p.substring(4,5));
			sum += 3*Integer.parseInt(p.substring(5,6));
			sum += 7*Integer.parseInt(p.substring(6,7));
			sum += 9*Integer.parseInt(p.substring(7,8));
			sum += 1*Integer.parseInt(p.substring(8,9));
			sum += 3*Integer.parseInt(p.substring(9,10));
			sum += Integer.parseInt(p.substring(10,11));
			if (sum % 10 == 0) {
				validated = true;
			}
		}
		if (!validated) {
	       FacesMessage message = new FacesMessage();
	       message.setDetail("Nieprawidłowy PESEL.");
	       message.setSummary("Nieprawidłowy PESEL.");
	       message.setSeverity(FacesMessage.SEVERITY_ERROR);
	       throw new ValidatorException(message);
		}
	}
}
