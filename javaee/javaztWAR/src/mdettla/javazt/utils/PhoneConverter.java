package mdettla.javazt.utils;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import mdettla.javazt.entities.PhoneNumber;

public class PhoneConverter implements Converter {

	@Override
	public Object getAsObject(FacesContext context, UIComponent component, String value)
			throws ConverterException {
		PhoneNumber phone = new PhoneNumber();
		if (value.equals("")) {
			return phone;
		}
		value = value.replaceAll("\\D", " ").replaceAll(" +", " ").trim();
		String[] phoneComponents = value.split(" ");
		
		if (phoneComponents.length == 1) {
		    FacesMessage message = new FacesMessage();
		    message.setDetail("Musisz podać prefiks.");
		    message.setSummary("Nieprawidłowy telefon.");
		    message.setSeverity(FacesMessage.SEVERITY_ERROR);
		    throw new ConverterException(message);
		} else if (phoneComponents.length == 2) {
			phone.setPrefix(phoneComponents[0]);
			phone.setNumber(phoneComponents[1]);
		} else if (phoneComponents.length > 2) {
			phone.setAreaCode(phoneComponents[0]);
			phone.setPrefix(phoneComponents[1]);
			phone.setNumber(phoneComponents[2]);
		}
		return phone;
	}

	@Override
	public String getAsString(FacesContext context, UIComponent component, Object value)
			throws ConverterException {
		if (value == null) {
			return "";
		} else {
			return value.toString();
		}
	}
}
