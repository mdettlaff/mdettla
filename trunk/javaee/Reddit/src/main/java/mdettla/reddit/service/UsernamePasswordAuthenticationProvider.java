package mdettla.reddit.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Service;

@Service
// TODO delete this class if UserDetailsForAuthenticationService will be sufficient
public class UsernamePasswordAuthenticationProvider implements AuthenticationProvider {

	@Override
	public Authentication authenticate(Authentication authentication)
	throws AuthenticationException {
		UsernamePasswordAuthenticationToken token =
			(UsernamePasswordAuthenticationToken)authentication;
		String username = token.getName();
		String password = (String)token.getCredentials();
		verifyUsernameAndPassword(username, password);
		return createAuthenticatedToken(username);
	}

	private void verifyUsernameAndPassword(String username, String password) {
		if (!getUsernamesAndPasswords().containsKey(username)) {
			throw new BadCredentialsException("Unknown username: " + username);
		}
		if (!getUsernamesAndPasswords().get(username).equals(password)) {
			throw new BadCredentialsException("Invalid password");
		}
	}

	private UsernamePasswordAuthenticationToken createAuthenticatedToken(
			String username) {
		Collection<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
		authorities.add(new SimpleGrantedAuthority("user"));
		if ("admin".equals(username)) {
			authorities.add(new SimpleGrantedAuthority("admin"));
		}
		UsernamePasswordAuthenticationToken authenticated =
			new UsernamePasswordAuthenticationToken(null, null, authorities);
		return authenticated;
	}

	private Map<String, String> getUsernamesAndPasswords() {
		Map<String, String> usernamesAndPasswords = new HashMap<String, String>();
		usernamesAndPasswords.put("mdettla", "secret");
		usernamesAndPasswords.put("admin", "secret1");
		return usernamesAndPasswords ;
	}

	@Override
	public boolean supports(Class<?> authentication) {
		return UsernamePasswordAuthenticationToken.class.equals(authentication);
	}
}
