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

	<c:set var="p_index" scope="page" value="${customer.productId-1}"></c:set>
	<c:if test="${customer.productId > 0}">
		Dodałeś do koszyka produkt:<br/>
    	${customer.products[p_index].title}
		(${customer.products[p_index].priceZloty}.${customer.products[p_index].priceGrosz} zł)
    	<br/><hr>
    </c:if>
    Zawartość koszyka:<br/>
	<my:koszyk/>
	<c:if test="${!empty customer.cart}">
	    <br/><hr>
   		<h:outputLink value="order.jsp">Zamów produkty</h:outputLink>
   	</c:if>
    <jsp:setProperty name="customer" property="productId" value="0" />
	<jsp:setProperty name="customer" property="productQuantity" value="1" />
	
	<my:menu/>
</f:view>
</body>
</html>