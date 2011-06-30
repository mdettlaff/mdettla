<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>

<p>
Title: <c:out value="${submission.title}" />
</p><p>
<a href="<c:url value="/submissions/${submission.id}/edit" />">Edit</a>
</p>
