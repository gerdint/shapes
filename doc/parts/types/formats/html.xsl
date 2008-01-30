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
			<p><b>Sections:</b>
				<xsl:for-each select="section">
					  
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@id" /></xsl:attribute>
						<xsl:value-of select="title" />
					</xsl:element>
				</xsl:for-each>
			</p>

			<hr class="thin"/>
			<p class="center"><b>Alphabetical list</b></p>
			<p class="center">
				<xsl:for-each select="/book/section/coretype[@name]">
					<xsl:sort select="@name" />
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@name" /></xsl:attribute>
						§<xsl:value-of select="@name" />
					</xsl:element>
					  
				</xsl:for-each>
			</p>
			<hr class="thin"/>

			<xsl:for-each select="section">
				<h2>
					<xsl:element name="a">
						<xsl:attribute name="name"><xsl:value-of select="@id" /></xsl:attribute>
						<xsl:value-of select="title" />
					</xsl:element>
				</h2>
				<xsl:apply-templates select="top" />
				<p class="center">
					<xsl:for-each select="coretype[@name]">
						<xsl:sort select="@name" />
						<xsl:element name="a">
							<xsl:attribute name="href">#<xsl:value-of select="@name" /></xsl:attribute>
							§<xsl:value-of select="@name" />
						</xsl:element>
						  
					</xsl:for-each>
				</p>
				<xsl:apply-templates select="coretype" />
			</xsl:for-each>

		</body>
  </html>
</xsl:template>

<xsl:template match="coretype[@name]">
  <xsl:variable name="self">
    <xsl:value-of select="@name" />
  </xsl:variable>
	<h3>
		<xsl:element name="a">
			<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
			§<xsl:value-of select="@name" />
		</xsl:element>
	</h3>
	<xsl:apply-templates select="abstraction" />
	<xsl:apply-templates select="construction" />
	<xsl:apply-templates select="description" />
</xsl:template>

<xsl:template match="coretype[@name]/abstraction">
	<h4>Abstraction</h4>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="coretype[@name]/construction">
	<h4>Construction</h4>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="coretype[@name]/description">
	<h4>Description</h4>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="coretype[@name]/abstraction/p/self">
	<typename>§<xsl:value-of select="../../../@name" /></typename>
</xsl:template>

</xsl:stylesheet>
