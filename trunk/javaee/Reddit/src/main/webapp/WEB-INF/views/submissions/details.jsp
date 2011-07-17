<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<h2>${submission.title}</h2>

score: ${submission.score}<br>
author: ${submission.author.name}

<form:form action="${submission.id}/upvote">
	<input type="submit" value="&#8679;" style="background: none; border-width: 0px; color: blue;" />
</form:form>

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
