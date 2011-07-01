<%@ page language="java" contentType="text/html; charset=UTF-8" pageEncoding="UTF-8"%>
<%@ taglib prefix="c" uri="http://java.sun.com/jsp/jstl/core"%>
<%@ taglib prefix="tiles" uri="http://tiles.apache.org/tags-tiles"%>

<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<html>
	<head>
		<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
		<title>Reddit</title>
		<link rel="stylesheet" href="<c:url value="/resources/css/style.css" />" type="text/css" />
		<link rel="alternate" href="<c:url value="/rss" />" type="application/rss+xml" title="RSS" />
	</head>
	<body>
		<div class="page">
			<div class="header">
				<div id="title" style="background-image:
						url('<c:url value="/resources/images/alien.png" />');">
					<h1>Reddit</h1>
				</div>
				<div id="login">
					[ <a href="<c:url value="/login" />">Login</a> ]
				</div>
				<div class="menu-container">
					<ul id="menu">
						<li><a href="<c:url value="/" />">Home page</a></li>
						<li><a href="<c:url value="/submissions/add" />">Submit link</a></li>
						<li><a href="<c:url value="/rss" />">RSS feed</a></li>
					</ul>
				</div>
			</div>
			<div class="content">

				<tiles:insertAttribute name="content" />

			</div>
			<div class="footer"></div>
		</div>
	</body>
</html>
