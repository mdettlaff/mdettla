<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <!--
  zadanie 3: zdefiniowac przeksztalcenie XSLT, ktore dokonuje transformacji do
  dokumentu XHTML w podanej w zalaczniku postaci
  -->
  <xsl:template match="/">
    <html>
      <body>
	<center>
	  Numer faktury: <xsl:value-of select="faktura/Nrfaktury"/><br />
	  Sprzedawca: <xsl:value-of select="faktura/Sprzedawca/NazwaFirmy"/>
	  <br />
	  Nabywca: <xsl:value-of select="faktura/Nabywca/NazwiskoOsoby"/><br />
	  <br />
	<table border="1">
	  <tr>
	    <th>Towar</th>
	    <th>Cena</th>
	    <th>Ilosc</th>
	    <th>Razem</th>
	  </tr>
	  <xsl:for-each select="faktura/lista/towar">
	    <tr>
	      <td><xsl:value-of select="nazwa"/></td>
	      <td><xsl:value-of select="cena"/></td>
	      <td><xsl:value-of select="ilosc"/></td>
	      <td><xsl:value-of select="cena * ilosc"/></td>
	    </tr>
	  </xsl:for-each>
	</table>
	</center>
      </body>
    </html>
  </xsl:template>
</xsl:stylesheet>
