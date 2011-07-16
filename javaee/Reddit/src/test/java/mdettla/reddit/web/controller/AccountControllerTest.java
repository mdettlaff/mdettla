package mdettla.reddit.web.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import mdettla.reddit.domain.DuplicateUsernameException;
import mdettla.reddit.domain.User;
import mdettla.reddit.service.AccountService;
import mdettla.reddit.web.form.RegisterForm;

import org.junit.Before;
import org.junit.Test;
import org.springframework.ui.ExtendedModelMap;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;

public class AccountControllerTest {

	private AccountService accountService;
	private AccountController controller;

	@Before
	public void setUp() {
		accountService = mock(AccountService.class);
		controller = new AccountController(accountService);
	}

	@Test
	public void testSetupFormRegister() {
		// prepare
		Model model = new ExtendedModelMap();
		// test
		controller.setupFormRegister(model);
		// verify
		RegisterForm form = (RegisterForm)model.asMap().get("registerForm");
		assertNotNull(form);
	}

	@Test
	public void testRegister() throws DuplicateUsernameException {
		// mock
		BindingResult result = mock(BindingResult.class);
		// prepare
		RegisterForm form = new RegisterForm();
		form.setName("johndoe");
		form.setPassword("foo");
		// test
		String viewName = controller.register(form, result);
		// verify
		assertEquals("redirect:login", viewName);
		verify(accountService).createUser(new User("johndoe", "foo"));
	}
}
