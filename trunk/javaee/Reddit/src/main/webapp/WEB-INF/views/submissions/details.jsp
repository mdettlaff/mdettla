<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<h2>${submission.title}</h2>

<table>
	<tr>
		<td><a href="${submission.id}/edit">Edit</a></td>
		<td>
			<form:form action="${submission.id}/delete">
				<input type="submit" value="Delete" />
			</form:form>
		</td>
	</tr>
</table>
