<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib prefix="f"  uri="http://java.sun.com/jsf/core"%>
<%@ taglib prefix="h"  uri="http://java.sun.com/jsf/html"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="my" tagdir="/WEB-INF/tags/" %> 
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
<%
	request.setAttribute("product_id", request.getParameter("product_id"));
%>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<link rel="stylesheet" type="text/css" href="style.css" media="screen" />
<title>Sklep internetowy</title>
</head>
<body>
<f:view>
	<jsp:include page="include/begin.jspf"></jsp:include>

	<jsp:setProperty name="customer" property="productId" value="${requestScope.product_id}" />
	<jsp:setProperty name="customer" property="productQuantity" value="1" />
	<c:set var="p_index" scope="page" value="${customer.productId-1}"></c:set>
    <h2>${customer.products[p_index].title}</h2>
    <p>
	Cena: ${customer.products[p_index].priceZloty}.${customer.products[p_index].priceGrosz} zł
	</p><p>
    Opis: ${customer.products[p_index].description}
    </p><p>
    <h:form>
		Ilość egzemplarzy do zakupu: <h:inputText value="#{customer.productQuantity}"
			style="width: 25px;" id="ilosc_egz"/>
		<br/>
		<h:commandButton value="Dodaj produkt do koszyka" action="#{customer.addProductToCart}"/>
	</h:form>
	</p>
	
	<my:menu/>
</f:view>
</body>
</html>