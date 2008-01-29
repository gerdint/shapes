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
				<xsl:for-each select="/book/section/system-binding[@identifier]">
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
					<xsl:for-each select="system-binding[@identifier]">
						<xsl:sort select="@identifier" />
						<xsl:element name="a">
							<xsl:attribute name="href">#<xsl:value-of select="@identifier" /></xsl:attribute>
							<xsl:value-of select="@identifier" />
						</xsl:element>
						  
					</xsl:for-each>
				</p>
				<xsl:apply-templates select="system-binding" />
			</xsl:for-each>
		</body>
  </html>
</xsl:template>

<xsl:template match="system-binding[@identifier]">
	<h3>
		<xsl:element name="a">
			<xsl:attribute name="name"><xsl:value-of select="@identifier" /></xsl:attribute>
			<xsl:value-of select="@identifier" />
		</xsl:element>
	</h3>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="system-binding[@identifier]/function">
 	<xsl:apply-templates select="case"/>
</xsl:template>

<xsl:template match="system-binding[@identifier]/function/case[@constructor-of]">
	<h4 class="plain">
		<b>Case</b>  
 		<xsl:apply-templates select="arguments"/>→ <typename><xsl:value-of select="@constructor-of" /></typename>
	</h4>
 	<xsl:apply-templates select="dynamic-references"/>
 	<xsl:apply-templates select="description"/>
</xsl:template>

<xsl:template match="function/case/arguments/arg[@identifier]">
	<paramname><xsl:value-of select="@identifier" /></paramname>
	<xsl:apply-templates select="default"/>
	<xsl:apply-templates select="type"/>
	<xsl:text> </xsl:text>
</xsl:template>
<xsl:template match="function/case/arguments/arg[not(@identifier)]">
	<xsl:apply-templates select="default"/>
	<xsl:apply-templates select="type"/>
	<xsl:text> </xsl:text>
</xsl:template>
<xsl:template match="function/case/arguments/arg/default">:<xsl:apply-templates /></xsl:template>
<xsl:template match="function/case/arguments/arg/type"><xsl:text>::</xsl:text><xsl:apply-templates /></xsl:template>

<xsl:template match="dynamic-references[not(dynvar)]">
	<p><b>Dynamic references:</b><xsl:text> </xsl:text><em>none</em></p>
</xsl:template>
<xsl:template match="dynamic-references[dynvar]">
	<p><b>Dynamic references:</b><xsl:text> </xsl:text>
		<xsl:apply-templates />
	</p>
</xsl:template>

</xsl:stylesheet>
