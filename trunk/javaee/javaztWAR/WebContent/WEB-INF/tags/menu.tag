<%@ tag body-content="empty" %> 
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core" %>
<%@ taglib prefix="h"  uri="http://java.sun.com/jsf/html"%>

	</div>
	</div>
	<div id="left"> 

<h3>Oferta</h3>
<ul>
	<li><h:outputLink value="index.jsp">Home</h:outputLink></li>
<li>Kategorie:</li>
	<li><h:outputLink value="products.jsp?cat=movie">Filmy DVD</h:outputLink></li>
	<li><h:outputLink value="products.jsp?cat=music">Muzyka</h:outputLink></li>
	<li><h:outputLink value="products.jsp?cat=books">Literatura</h:outputLink></li>

</ul>
<h3>Uzytkownik</h3>
<ul>
	<li><h:outputLink value="cart.jsp">Zobacz koszyk</h:outputLink></li>
<c:if test="${!empty customer.cart}">
	<li><h:outputLink value="order.jsp">Zamow produkty</h:outputLink></li>
</c:if>

<c:choose>
<c:when test="<%= request.getUserPrincipal() != null %>">
	<li><% out.write("Zalogowany jako " + request.getUserPrincipal().getName()); %>
	(<h:outputLink value="logout.jsp">wyloguj</h:outputLink>)</li><br/>
	<li><h:outputLink value="profile.jsp">Dane uzytkownika</h:outputLink></li>
</c:when>
<c:otherwise>
	<li><h:outputLink value="loginWelcome.jsp">Zaloguj sie</h:outputLink></li>
	<li><h:outputLink value="register.jsp">Rejestracja</h:outputLink></li>
</c:otherwise>
</c:choose>
</ul>

	</div>
	<div style="clear: both;"> </div>
		<div id="footer">
			Copyright M. Dettlaff - <h:outputLink value="http://manta.univ.gda.pl/~mdettla/">
			manta.univ.gda.pl/~mdettla</h:outputLink>
		</div>
	</div>
