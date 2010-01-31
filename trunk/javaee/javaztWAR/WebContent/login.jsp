<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib prefix="f"  uri="http://java.sun.com/jsf/core"%>
<%@ taglib prefix="h"  uri="http://java.sun.com/jsf/html"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="my" tagdir="/WEB-INF/tags/" %> 
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<link rel="stylesheet" type="text/css" href="style.css" media="screen" />
<title>Sklep internetowy</title>
</head>
<body>
<f:view>
	<jsp:include page="include/begin.jspf"></jsp:include>

	<form method="POST" action="j_security_check">
		Login:<br>
		<input size="20" name="j_username">
		<br><br>
		Hasło:<br>
		<input size="20" name="j_password" type="password">
		<br/>
		<input name="submit" type="submit" value="Login">
	</form>
	<br/>
	Nie masz jeszcze u nas konta?
	<h:outputLink value="register.jsp">Zarejestruj się</h:outputLink>

	<my:menu/>
</f:view>
</body>
</html>
