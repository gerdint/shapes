<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" indent="no" />

<xsl:include href="../../formats/html.xsl" />
<xsl:include href="../../formats/examplecode-html.xsl" />

<xsl:template match="/book">
  <html>
    <head>
      <title><xsl:apply-templates select="title" /></title>
      <link rel="stylesheet" href="../../styles/html/shapes.css" />
    </head>
    <body>
    <h0><xsl:apply-templates select="title" /></h0>
    <hr class="thick"/>
    <xsl:apply-templates select="top" />
    <xsl:for-each select="section">
      <h1><xsl:apply-templates select="title" /></h1>
      <xsl:apply-templates select="top" />
      <xsl:apply-templates select="body" />
      <xsl:for-each select="section">
				<h2><xsl:apply-templates select="title" /></h2>
				<xsl:apply-templates select="top" />
				<xsl:apply-templates select="body" />
				<xsl:for-each select="section">
					<h3><xsl:apply-templates select="title" /></h3>
					<xsl:apply-templates select="top" />
					<xsl:apply-templates select="body" />
				</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </body>
  </html><xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="secref-title[@id]">
  <xsl:variable name="dstid">
    <xsl:value-of select="@id" />
  </xsl:variable>
  <xsl:element name="a">
    <xsl:attribute name="href">#sec-<xsl:value-of select="@id" /></xsl:attribute>
    <xsl:apply-templates select="//section[@id=$dstid]/title" />
  </xsl:element>
</xsl:template>
<xsl:template match="secref[@id]">
  <xsl:element name="a">
    <xsl:attribute name="href">#sec-<xsl:value-of select="@id" /></xsl:attribute>
    <xsl:apply-templates />
  </xsl:element>
</xsl:template>
<xsl:template match="secref/title">
  <xsl:variable name="dstid">
    <xsl:value-of select="../@id" />
  </xsl:variable>
  <xsl:apply-templates select="//section[@id=$dstid]/title" />
</xsl:template>

</xsl:stylesheet>

