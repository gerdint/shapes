<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" indent="no" />

<xsl:include href="../../formats/html.xsl" />

<xsl:template match="/book">
  <html>
    <head>
      <title><xsl:value-of select="title" /></title>
      <link rel="stylesheet" href="../../styles/html/shapes.css" />
    </head>
    <body>
			<h1><xsl:value-of select="title" /></h1>
			<hr class="thick"/>
			<xsl:apply-templates select="top" />

			<p class="center">
				<xsl:for-each select="/book/dynamic-variable[@identifier]">
					<xsl:sort select="@identifier" />
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@identifier" /></xsl:attribute>
						@<xsl:value-of select="@identifier" />
					</xsl:element>
					  
				</xsl:for-each>
			</p>

			<xsl:apply-templates select="/book/dynamic-variable[@identifier]" />

		</body>
  </html>
</xsl:template>

<xsl:template match="/book/dynamic-variable[@identifier]">
  <xsl:variable name="self">
    <xsl:value-of select="@identifier" />
  </xsl:variable>
	<hr class="thin"/>
	<h2>
		<xsl:element name="a">
			<xsl:attribute name="name"><xsl:value-of select="@identifier" /></xsl:attribute>
			@<xsl:value-of select="@identifier" />
		</xsl:element>
	</h2>
	<p><b>Used by: </b>
		<xsl:for-each select="/book/section/system-binding[@identifier]">
			<xsl:if test="function/case/dynamic-references/dynvar[@name=$self]">
				<xsl:element name="a">
					<xsl:attribute name="href">bindings#<xsl:value-of select="@identifier" /></xsl:attribute>
					<varname><xsl:value-of select="@identifier" /></varname>
				</xsl:element>
			</xsl:if>
		</xsl:for-each>
	</p>
	<xsl:apply-templates select="constraint" />
	<xsl:apply-templates select="description" />
</xsl:template>

<xsl:template match="/book/dynamic-variable[@identifier]/constraint">
	<p>
		<b>Constraint: </b><xsl:apply-templates />
	</p>
</xsl:template>

<xsl:template match="/book/dynamic-variable[@identifier]/description">
	<h3>Description</h3>
	<xsl:apply-templates />
</xsl:template>

</xsl:stylesheet>
