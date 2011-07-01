<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<h2>${submission.title}</h2>

<a href="${submission.id}/edit">Edit</a>
<form:form action="${submission.id}/delete">
	<input type="submit" value="Delete" />
</form:form>
