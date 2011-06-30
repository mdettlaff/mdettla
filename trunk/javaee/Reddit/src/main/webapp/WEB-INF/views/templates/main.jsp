<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="tiles" uri="http://tiles.apache.org/tags-tiles"%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
		<title>Reddit</title>
	</head>
	<body>
		<ul>
			<li><a href="<c:url value="/" />">Home page</a></li>
			<li><a href="<c:url value="/submissions/add" />">Submit link</a></li>
			<li><a href="<c:url value="/rss" />">RSS feed</a></li>
		</ul>

		<tiles:insertAttribute name="content" />

	</body>
</html>
