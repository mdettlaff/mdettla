package mdettla.reddit.validation;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.util.ObjectUtils;

public final class ConfirmValidator implements ConstraintValidator<Confirm, Object> {

    private String field;
    private String matches;
    private String message;

	@Override
	public void initialize(Confirm constraintAnnotation) {
		field = constraintAnnotation.field();
		matches = constraintAnnotation.matches();
		message = constraintAnnotation.message();
	}

	@Override
	public boolean isValid(Object value, ConstraintValidatorContext context) {
		BeanWrapper beanWrapper = new BeanWrapperImpl(value);
		Object fieldValue = beanWrapper.getPropertyValue(field);
		Object matchesValue = beanWrapper.getPropertyValue(matches);
		boolean matched = ObjectUtils.nullSafeEquals(fieldValue, matchesValue);
		if (matched) {
			return true;
		} else {
			context.disableDefaultConstraintViolation();
			context.buildConstraintViolationWithTemplate(message)
				.addNode(field).addConstraintViolation();
			return false;
		}
	}
}
