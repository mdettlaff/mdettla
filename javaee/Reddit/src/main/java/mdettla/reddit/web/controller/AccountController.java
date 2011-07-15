package mdettla.reddit.web.controller;

import mdettla.reddit.domain.User;
import mdettla.reddit.service.AccountService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
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
		model.addAttribute(new User());
	}

	@RequestMapping(value = "/register", method = RequestMethod.POST)
	public String register(User user) {
		accountService.createUser(user);
		return "redirect:login";
	}
}
