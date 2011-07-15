<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<form:form modelAttribute="user">
	<table>
		<tr>
			<td>Username:</td>
			<td><form:input path="name" /></td>
		</tr>
		<tr>
			<td>Password:</td>
			<td><form:password path="password" /></td>
		</tr>
		<tr>
			<td></td>
			<td><input type="submit" value="Register" /></td>
		</tr>
	</table>
</form:form>
