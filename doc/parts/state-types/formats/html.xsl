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
			<h1><xsl:apply-templates select="title" /></h1>
			<hr class="thick"/>
			<xsl:apply-templates select="top" />
			<p><b>Sections:</b>
				<xsl:for-each select="section">
					  
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@id" /></xsl:attribute>
						<xsl:apply-templates select="title" />
					</xsl:element>
				</xsl:for-each>
			</p>

			<hr class="thin"/>
			<p class="center"><b>Alphabetical list</b></p>
			<p class="center">
				<xsl:for-each select="/book/section/core-state-type[@name]">
					<xsl:sort select="@name" />
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@name" /></xsl:attribute>
						<xsl:call-template name="name-to-state-type"><xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param></xsl:call-template>
					</xsl:element>
					  
				</xsl:for-each>
			</p>
			<hr class="thin"/>

			<xsl:for-each select="section">
				<h2>
					<xsl:element name="a">
						<xsl:attribute name="name"><xsl:value-of select="@id" /></xsl:attribute>
						<xsl:apply-templates select="title" />
					</xsl:element>
				</h2>
				<xsl:apply-templates select="top" />
				<p class="center">
					<xsl:for-each select="core-state-type[@name]">
						<xsl:sort select="@name" />
						<xsl:element name="a">
							<xsl:attribute name="href">#<xsl:value-of select="@name" /></xsl:attribute>
							<xsl:call-template name="name-to-state-type"><xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param></xsl:call-template>
						</xsl:element>
						  
					</xsl:for-each>
				</p>
				<xsl:apply-templates select="core-state-type" />
			</xsl:for-each>

		</body>
  </html>
</xsl:template>

<xsl:template match="core-state-type[@name]">
  <xsl:variable name="self">
    <xsl:value-of select="@name" />
  </xsl:variable>
	<h3>
		<xsl:element name="a">
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			State type <xsl:call-template name="name-to-state-type"><xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param></xsl:call-template>
		</xsl:element>
	</h3>
	<xsl:apply-templates select="abstraction" />
	<h4>Construction</h4>
	<xsl:apply-templates select="description" />
</xsl:template>

<xsl:template match="core-state-type[@name]/abstraction">
	<h4>Abstraction</h4>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="core-state-type[@name]/description">
	<h4>Description</h4>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="core-state-type[@name]/abstraction/p/self">
	<xsl:call-template name="name-to-state-type">
		<xsl:with-param name="name"><xsl:value-of select="../../../@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="core-state-type[@name]/description/p/self">
	<xsl:call-template name="name-to-state-type">
		<xsl:with-param name="name"><xsl:value-of select="../../../@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

</xsl:stylesheet>
