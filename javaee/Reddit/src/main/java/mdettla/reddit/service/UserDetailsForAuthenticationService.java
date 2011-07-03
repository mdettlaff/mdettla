package mdettla.reddit.service;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
public class UserDetailsForAuthenticationService implements UserDetailsService {

	@Override
	public UserDetails loadUserByUsername(String username)
			throws UsernameNotFoundException {
		UserDetails userDetails = getUsersDatabase().get(username);
		if (userDetails == null) {
			return new User(username, "", createAuthorities());
		}
		return userDetails;
	}

	private Map<String, UserDetails> getUsersDatabase() {
		Map<String, UserDetails> users = new HashMap<String, UserDetails>();
		users.put("mdettla", new User("mdettla", "secret", createAuthorities("user")));
		users.put("admin", new User("admin", "secret1", createAuthorities("user", "admin")));
		return users;
	}

	private Collection<GrantedAuthority> createAuthorities(String... roles) {
		Collection<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
		for (String role : roles) {
			authorities.add(new SimpleGrantedAuthority(role));
		}
		return authorities;
	}
}
