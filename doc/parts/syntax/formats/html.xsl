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
    <h1><xsl:apply-templates select="title" /></h1>
    <hr class="thick"/>
    <xsl:apply-templates select="top" />
    <xsl:for-each select="section">
      <h2><xsl:apply-templates select="title" /></h2>
      <xsl:apply-templates select="top" />
      <xsl:apply-templates select="body" />
      <xsl:for-each select="section">
				<h3><xsl:apply-templates select="title" /></h3>
				<xsl:apply-templates select="top" />
				<xsl:apply-templates select="body" />
				<xsl:for-each select="section">
					<h4><xsl:apply-templates select="title" /></h4>
					<xsl:apply-templates select="top" />
					<xsl:apply-templates select="body" />
				</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
		</body>
  </html><xsl:text>
	</xsl:text>
</xsl:template>

<xsl:template match="syntax-table">
  <table cellspacing="5"><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="token-example-table">
  <table cellspacing="5">
    <xsl:apply-templates select="head"/>
    <xsl:apply-templates select="body"/>
  </table>
</xsl:template>

<xsl:template match="token-example-table/head/tr">
  <tr align="left"><xsl:apply-templates/></tr>
</xsl:template>

</xsl:stylesheet>

