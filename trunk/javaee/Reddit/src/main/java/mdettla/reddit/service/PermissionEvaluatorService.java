package mdettla.reddit.service;

import java.io.Serializable;

import mdettla.reddit.domain.Submission;

import org.springframework.security.access.PermissionEvaluator;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Service;

@Service
public class PermissionEvaluatorService implements PermissionEvaluator {

	@Override
	public boolean hasPermission(Authentication authentication,
			Object targetDomainObject, Object permission) {
		if (isUserInRole(authentication, "administrator")) {
			return true;
		}
		String username = authentication.getName();
		if (targetDomainObject instanceof Submission) {
			Submission submission = (Submission)targetDomainObject;
			if ("update".equals(permission)) {
				return isUpdaterOfSubmissionAlsoItsAuthor(username, submission);
			}
			throw new IllegalArgumentException("Unknown permission: " + permission);
		}
		throw new IllegalArgumentException("Unknown domain object: " + targetDomainObject);
	}

	private boolean isUserInRole(Authentication authentication, String role) {
		return authentication.getAuthorities().contains(new SimpleGrantedAuthority(role));
	}

	private boolean isUpdaterOfSubmissionAlsoItsAuthor(
			String username, Submission submission) {
		return submission.getAuthor().getName().equals(username);
	}

	@Override
	public boolean hasPermission(Authentication authentication,
			Serializable targetId, String targetType, Object permission) {
		throw new UnsupportedOperationException();
	}
}
