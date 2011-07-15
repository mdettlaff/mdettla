<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<%@ attribute name="submitButtonLabel" type="java.lang.String" required="true"%>

<form:form modelAttribute="submission">
	<table>
		<tr>
			<td>Title:</td>
			<td><form:input path="title" /></td>
		</tr>
		<tr>
			<td></td>
			<td><input type="submit" value="${submitButtonLabel}" /></td>
		</tr>
	</table>
</form:form>
