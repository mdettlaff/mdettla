<%@ include file="/WEB-INF/view/layout/header.jsp" %>

Submitted links:<br>
<c:forEach var="submission" items="${submissions}">
	<c:out value="${submission.title}" /><br>
</c:forEach>

<%@ include file="/WEB-INF/view/layout/footer.jsp" %>
