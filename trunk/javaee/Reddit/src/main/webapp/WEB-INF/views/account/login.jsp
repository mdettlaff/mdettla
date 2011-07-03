<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<form action="<c:url value="/login/authenticate" />" method="post">
	<c:if test="${not empty param.error}">
		<span class="field-validation-error">
			Incorrect login or password.
		</span>
	</c:if>
	<table>
		<tr>
			<td>Username:</td>
			<td><input name="j_username" type="text" /></td>
		</tr>
		<tr>
			<td>Password:</td>
			<td><input name="j_password" type="password" /></td>
		</tr>
		<tr>
			<td></td>
			<td><button type="submit">Login</button></td>
		</tr>
	</table>
</form>
