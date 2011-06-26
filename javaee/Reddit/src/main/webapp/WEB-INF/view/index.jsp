<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<h2>Submitted links</h2>

<ul>
	<c:forEach var="submission" items="${submissions}">
		<li><c:out value="${submission.title}" /></li>
	</c:forEach>
</ul>
