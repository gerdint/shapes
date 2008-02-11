<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" indent="no" />

<xsl:include href="../../formats/html.xsl" />
<xsl:include href="../../formats/examplecode-html.xsl" />

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
				<xsl:for-each select="/book/section/system-state[@identifier]">
					<xsl:sort select="@identifier" />
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@identifier" /></xsl:attribute>
						<xsl:value-of select="@identifier" />
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
					<xsl:for-each select="system-state[@identifier]">
						<xsl:sort select="@identifier" />
						<xsl:element name="a">
							<xsl:attribute name="href">#<xsl:value-of select="@identifier" /></xsl:attribute>
							<xsl:value-of select="@identifier" />
						</xsl:element>
						  
					</xsl:for-each>
				</p>
				<xsl:apply-templates select="system-state" />
			</xsl:for-each>
		</body>
  </html>
</xsl:template>

<xsl:template match="system-state[@identifier]">
	<h3>
		<xsl:element name="a">
			<xsl:attribute name="name"><xsl:value-of select="@identifier" /></xsl:attribute>
			<xsl:call-template name="name-to-state"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
		</xsl:element>
	</h3>
	<p>
		<b>Type:</b><xsl:text> </xsl:text><xsl:apply-templates select="type"/>
	</p>
	<h4>Description</h4>
	<xsl:apply-templates select="description/*"/>
</xsl:template>


</xsl:stylesheet>
