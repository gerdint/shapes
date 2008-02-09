<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" indent="no" />

<xsl:include href="../../formats/html.xsl" />

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

<xsl:template match="index-of-books">
	<ul>
		<xsl:apply-templates select="/book/external/book | /book/external/man" />
	</ul>
</xsl:template>

<xsl:template match="external/book">
	<li>
		<xsl:element name="a">
			<xsl:attribute name="href"><xsl:value-of select="meta-filename" /></xsl:attribute>
			<b><xsl:apply-templates select="title" /></b>
		</xsl:element>:
		<xsl:apply-templates select="description" />
	</li>
</xsl:template>

<xsl:template match="external/man">
	<li>
		<xsl:element name="a">
			<xsl:attribute name="href"><xsl:value-of select="manhead/meta-filename" /></xsl:attribute>
			<b><xsl:apply-templates select="manhead/center-header" /></b>
		</xsl:element>:
		<xsl:apply-templates select="manhead/description" />
	</li>
</xsl:template>

</xsl:stylesheet>

