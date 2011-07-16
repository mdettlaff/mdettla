<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="form" uri="http://www.springframework.org/tags/form" %>

<form:form modelAttribute="registerForm">
	<form:errors path="name" cssClass="field-validation-error" />
	<form:errors path="password" cssClass="field-validation-error" />
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
			<td>Confirm password:</td>
			<td><form:password path="confirmPassword" /></td>
		</tr>
		<tr>
			<td></td>
			<td><input type="submit" value="Register" /></td>
		</tr>
	</table>
</form:form>
