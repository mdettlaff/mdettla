package mdettla.reddit.web.controller;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import mdettla.reddit.domain.User;
import mdettla.reddit.service.AccountService;

import org.junit.Before;
import org.junit.Test;
import org.springframework.ui.ExtendedModelMap;
import org.springframework.ui.Model;

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
		User user = (User)model.asMap().get("user");
		assertNotNull(user);
	}

	@Test
	public void testRegister() {
		// prepare
		User user = new User();
		// test
		String viewName = controller.register(user);
		// verify
		assertEquals("redirect:login", viewName);
		verify(accountService).createUser(user);
	}
}
