<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<h2>Submitted links</h2>

<ul>
	<c:forEach var="submission" items="${submissions}">
		<li>
			<a href="<c:url value="/submissions/${submission.id}" />">
				${submission.title}
			</a>
		</li>
	</c:forEach>
</ul>
