package mdettla.reddit.security;

import java.util.ArrayList;
import java.util.Collection;

import mdettla.reddit.service.AccountService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

@Service
public class UserDetailsForAuthenticationService implements UserDetailsService {

	private final AccountService accountService;

	@Autowired
	public UserDetailsForAuthenticationService(AccountService accountService) {
		this.accountService = accountService;
	}

	@Override
	public UserDetails loadUserByUsername(String username)
			throws UsernameNotFoundException {
		mdettla.reddit.domain.User user = accountService.findUserByName(username);
		if (user != null) {
			Collection<GrantedAuthority> authorities = createAuthorities(user);
			return new User(user.getName(), user.getPassword(), authorities);
		}
		return new User(username, "", new ArrayList<GrantedAuthority>());
	}

	private Collection<GrantedAuthority> createAuthorities(mdettla.reddit.domain.User user) {
		Collection<GrantedAuthority> authorities = new ArrayList<GrantedAuthority>();
		if (user.isAdministrator()) {
			authorities.add(new SimpleGrantedAuthority("administrator"));
		}
		return authorities;
	}
}
