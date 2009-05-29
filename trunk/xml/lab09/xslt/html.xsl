<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="/">
    <html>
      <body>
	<table border="1">
	  <tr bgcolor="#9acd32">
	    <th>Title</th>
	    <th>Artist</th>
	  </tr>
	  <xsl:for-each select="catalog/cd">
	    <tr>
	      <td><xsl:value-of select="title"/></td>
	      <td><xsl:apply-templates select="artist"/></td>
	    </tr>
	  </xsl:for-each>
	</table>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="artist">
    Artist: <span style="color:#00ff00">
      <xsl:value-of select="."/></span>
    <br />
  </xsl:template>
</xsl:stylesheet>
