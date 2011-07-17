package mdettla.reddit.security;

import java.io.Serializable;

import mdettla.reddit.domain.Submission;
import mdettla.reddit.domain.User;

import org.springframework.security.access.PermissionEvaluator;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.stereotype.Component;

@Component
public class RedditPermissionEvaluator implements PermissionEvaluator {

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
				return hasPermissionToUpdateSubmission(username, submission);
			} else if ("vote".equals(permission)) {
				return hasPermissionToVote(username, submission);
			}
			throw new IllegalArgumentException("Unknown permission: " + permission);
		}
		throw new IllegalArgumentException("Unknown domain object: " + targetDomainObject);
	}

	private boolean isUserInRole(Authentication authentication, String role) {
		return authentication.getAuthorities().contains(new SimpleGrantedAuthority(role));
	}

	private boolean hasPermissionToUpdateSubmission(
			String username, Submission submission) {
		return submission.getAuthor().getName().equals(username);
	}

	private boolean hasPermissionToVote(String username, Submission submission) {
		return !hasUserAlreadyVoted(username, submission);
	}

	private boolean hasUserAlreadyVoted(String username, Submission submission) {
		for (User voter : submission.getVoters()) {
			if (username.equals(voter.getName())) {
				return true;
			}
		}
		return false;
	}

	@Override
	public boolean hasPermission(Authentication authentication,
			Serializable targetId, String targetType, Object permission) {
		throw new UnsupportedOperationException();
	}
}
