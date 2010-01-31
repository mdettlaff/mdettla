<%@ tag body-content="empty" %> 
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="h"  uri="http://java.sun.com/jsf/html"%>
<%@ taglib prefix="f"  uri="http://java.sun.com/jsf/core"%>
<c:choose>
	<c:when test="${!empty customer.cart}">

		<table border="0"><tr><td>
		
    	<table border="1" style="border-collapse: collapse" width="400">
		<tr><th><i>Nazwa:</i></th><th><i>Cena (zl):</i></th><th><i>Ilosc:</i></th></tr>
		<c:set var="i" value="0"></c:set>
	    <c:forEach var="cart_product" items="${customer.cart}">
  			<tr>
  				<td STYLE=" padding: 3px">${customer.products[cart_product.id-1].title}</td>
  				<td STYLE=" padding: 3px">${customer.products[cart_product.id-1].priceZloty}.
  										  ${customer.products[cart_product.id-1].priceGrosz} zl
  				</td>
  				<td STYLE=" padding: 3px">${cart_product.quantity}</td>
  			</tr>
			<c:set var="i" value="${i+1}"></c:set>
    	</c:forEach>
		</table>
		
		</td><td valign="bottom">
		<h:form>
    	<h:selectManyCheckbox value="#{customer.selectedToDeleteItems}" layout="pageDirection">
	    	<f:selectItems value="#{customer.cartSelectItems}" />
		</h:selectManyCheckbox>
		</td></tr></table>
		<h:commandButton value="Usun zaznaczone produkty" action="#{customer.deleteFromCart}"/>
		</h:form>
		
		<br/><br/>
		Wartosc wszystkich produktow z koszyka: ${customer.cartSum}
	</c:when>
	<c:otherwise>
		<i>koszyk jest pusty</i>
	</c:otherwise>
</c:choose>