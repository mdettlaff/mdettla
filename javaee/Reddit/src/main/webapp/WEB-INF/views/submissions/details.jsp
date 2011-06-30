<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<p>
	Title: <c:out value="${submission.title}" />
</p>
<p>
	<a href="<c:url value="${submission.id}/edit" />">Edit</a>
	<form:form action="${submission.id}/delete">
		<input type="submit" value="Delete" />
	</form:form>
</p>
