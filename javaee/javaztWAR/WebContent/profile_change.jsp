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
	customer.loadData();
%>
	<jsp:include page="include/begin.jspf"></jsp:include>

	Wprowadź nowe dane:<br/>
   	<h:form>

   	<h:panelGrid columns="3">
   	<h:panelGroup>
		Imię:</h:panelGroup><h:panelGroup>
		<h:inputText id="name" value="#{customer.name}" required="true"/>
		</h:panelGroup><h:panelGroup>
		<h:message for="name" style="color: red;"/>

		</h:panelGroup><h:panelGroup>Nazwisko:</h:panelGroup><h:panelGroup>
		<h:inputText id="surname" value="#{customer.surname}" required="true"/>
		</h:panelGroup><h:panelGroup>
		<h:message for="surname" style="color: red;"/>

		</h:panelGroup><h:panelGroup>Wiek:</h:panelGroup><h:panelGroup>
		<h:inputText id="age" value="#{customer.age}" required="true" >
		<f:validateLongRange minimum="18" maximum="120" /></h:inputText>
		</h:panelGroup><h:panelGroup>
		<h:message for="age" style="color: red;" showSummary="false"/>

		</h:panelGroup><h:panelGroup>Pesel:</h:panelGroup><h:panelGroup>
		<h:inputText id="pesel" value="#{customer.pesel}" required="true">
			<f:validator validatorId="javazt.utils.PESELValidator"/>
		</h:inputText></h:panelGroup><h:panelGroup>
		<h:message for="pesel" style="color: red;" showSummary="false"/> 

		</h:panelGroup><h:panelGroup>Telefon:</h:panelGroup><h:panelGroup>
		<h:inputText  id="phone" value="#{customer.phone}" required="true">
			<f:converter converterId="javazt.utils.PhoneConverter" />
		</h:inputText></h:panelGroup><h:panelGroup>
		<h:message for="phone" style="color: red;" showSummary="true"/>

		</h:panelGroup><h:panelGroup>Kod pocztowy:</h:panelGroup><h:panelGroup>
		<h:inputText id="zipCode" value="#{customer.zipCode}" required="true"/>
		</h:panelGroup><h:panelGroup>
		<h:message for="zipCode" style="color: red;"/>
		
		</h:panelGroup><h:panelGroup>Miasto:</h:panelGroup><h:panelGroup>
		<h:inputText id="town" value="#{customer.town}" required="true"/>
		</h:panelGroup><h:panelGroup>
		<h:message for="town" style="color: red;"/><br/>
		
		</h:panelGroup><h:panelGroup>Ulica:</h:panelGroup><h:panelGroup>
		<h:inputText id="street" value="#{customer.street}" required="true"/>
		</h:panelGroup><h:panelGroup>
		<h:message for="street" style="color: red;"/><br/>
	</h:panelGroup>
    </h:panelGrid>
      	<h:commandButton action="#{customer.update}" value="Aktualizuj dane"></h:commandButton>
   	</h:form>

	<my:menu/>
</f:view>
</body>
</html>