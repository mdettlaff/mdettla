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
<jsp:useBean id="customer" class="mdettla.javazt.mbeans.CustomerMB" scope="session" />
<%
	customer.setLogin(request.getUserPrincipal().getName());
%>
	<jsp:include page="include/begin.jspf"></jsp:include>

	<h3>Zamówienie</h3>
    <h:form>
		Podaj adres do wysyłki (jeśli inny niż w danych osobowych):<br/>
		<h:inputText id="sendAddress" value="#{customer.sendAddress}" required="false"/>
		<h:message for="sendAddress" style="color: red;"/><br/>
		<h:commandButton value="Złóż zamówienie" action="#{customer.makeOrder}"/>
	</h:form>
	<br/>
	Wybrane przedmioty:<br/>
	<my:koszyk/>
    
	<my:menu/>
</f:view>
</body>
</html>