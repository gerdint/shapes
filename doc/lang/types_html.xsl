<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="html"/>

<xsl:include href="../notation/html.xsl" />

<xsl:template match="/">
  <html>
  <body>
    <table border="2" bgcolor="yellow">
      <tr>
        <th>Type name</th>
        <th>Abstraction</th>
      </tr>
      <xsl:for-each select="coretypes/coretype">
      <tr>
        <td><xsl:value-of select="@name"/></td>
        <td><xsl:value-of select="abstraction"/></td>
      </tr>
      </xsl:for-each>
    </table>
  </body>
  </html>
</xsl:template>

</xsl:stylesheet>
