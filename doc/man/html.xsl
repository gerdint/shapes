<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/man">
  <html>
    <head>
      <title><xsl:value-of select="title" /></title>
      <link rel="stylesheet" href="../shapes.css" />
    </head>
    <body>
    <h0><xsl:value-of select="title" /></h0>
    <hr class="thick"/>
    <xsl:for-each select="section">
      <h1><xsl:value-of select="title" /></h1>
      <xsl:apply-templates select="top" />
      <xsl:apply-templates select="body" />
      <xsl:for-each select="section">
	<h2><xsl:value-of select="title" /></h2>
	<xsl:apply-templates select="top" />
	<xsl:apply-templates select="body" />
      </xsl:for-each>
    </xsl:for-each>
  </body>
  </html>
</xsl:template>


<xsl:template match="p">
  <p><xsl:apply-templates/></p>
</xsl:template>

</xsl:stylesheet>
