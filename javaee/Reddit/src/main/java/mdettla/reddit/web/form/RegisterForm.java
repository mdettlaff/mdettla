package mdettla.reddit.web.form;

import mdettla.reddit.validation.Confirm;

@Confirm(field = "password", matches = "confirmPassword",
		message = "Confirmed password does not match.")
public class RegisterForm {

	private String name;
	private String password;
	private String confirmPassword;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getConfirmPassword() {
		return confirmPassword;
	}

	public void setConfirmPassword(String confirmPassword) {
		this.confirmPassword = confirmPassword;
	}
}
