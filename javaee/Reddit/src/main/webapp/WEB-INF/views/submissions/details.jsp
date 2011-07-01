<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<p>
	Title: ${submission.title}
</p>
<p>
	<a href="${submission.id}/edit">Edit</a>
	<form:form action="${submission.id}/delete">
		<input type="submit" value="Delete" />
	</form:form>
</p>
