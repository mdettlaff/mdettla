<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="security" uri="http://www.springframework.org/security/tags"%>

<security:authorize access="!isAuthenticated()">
	[ <a href="<c:url value="/login" />">Login</a> ]
</security:authorize>
<security:authorize access="isAuthenticated()">
	[ <a href="<c:url value="/logout" />">Logout</a> ]
</security:authorize>
