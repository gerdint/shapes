<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:include href="../../formats/html.xsl" />

<xsl:template match="/book">
  <html>
    <head>
      <title><xsl:value-of select="title" /></title>
      <link rel="stylesheet" href="../../styles/html/shapes.css" />
    </head>
    <body>
			<h0><xsl:value-of select="title" /></h0>
			<hr class="thick"/>
			<xsl:apply-templates select="top" />
			<xsl:apply-templates select="body" />
		</body>
  </html>
</xsl:template>

<xsl:template match="alphabetical-list-of-links">
  <xsl:for-each select="system-binding[@identifier]">
		<xsl:element name="a">
			<xsl:attribute name="href">#<xsl:value-of select="@identifier" /></xsl:attribute>
			<xsl:value-of select="@identifier" />
		</xsl:element>
      
	</xsl:for-each>
</xsl:template>

<xsl:template match="alphabetical-list-of-bindings">
Foo
</xsl:template>

</xsl:stylesheet>
