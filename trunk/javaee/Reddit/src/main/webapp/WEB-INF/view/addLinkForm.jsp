<%@ include file="/WEB-INF/view/layout/header.jsp" %>

Add new link<br>

<form:form modelAttribute="submission" method="POST">
	Title: <form:input path="title" size="30" maxlength="80" />
	<br>
	<input type="submit" value="Submit"/>
</form:form>

<%@ include file="/WEB-INF/view/layout/footer.jsp" %>
