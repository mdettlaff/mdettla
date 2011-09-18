<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<table width="100%">
	<tr style="font-size: large">
		<td width="20">
			<form:form action="${submission.id}/upvote">
				<input type="submit" value="&#8679;" class="vote-arrow" />
			</form:form>
		</td>
		<td width="20">
			<form:form action="${submission.id}/downvote">
				<input type="submit" value="&#8681;" class="vote-arrow" />
			</form:form>
		</td>
		<td>
			(${submission.score})
			<a href="http://foo.com" target="_blank">
				${submission.title}
			</a>
			<br />
			<span style="font-size: medium">author: ${submission.author.name}</span>
		</td>
	</tr>
</table>

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
