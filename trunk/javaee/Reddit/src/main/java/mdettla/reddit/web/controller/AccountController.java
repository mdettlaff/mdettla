package mdettla.reddit.web.controller;

import javax.validation.Valid;

import mdettla.reddit.domain.User;
import mdettla.reddit.service.AccountService;
import mdettla.reddit.web.form.RegisterForm;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

@Controller
public class AccountController {

	private final AccountService accountService;

	@Autowired
	public AccountController(AccountService accountService) {
		this.accountService = accountService;
	}

	@RequestMapping(value = "/register", method = RequestMethod.GET)
	public void setupFormRegister(Model model) {
		model.addAttribute(new RegisterForm());
	}

	@RequestMapping(value = "/register", method = RequestMethod.POST)
	public String register(@Valid RegisterForm registerForm, BindingResult result) {
		if (result.hasErrors()) {
			return null;
		}
		User user = new User(registerForm.getName(), registerForm.getPassword());
		accountService.createUser(user);
		return "redirect:login";
	}
}
