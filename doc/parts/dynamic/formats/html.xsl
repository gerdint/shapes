<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" indent="no" />

<xsl:include href="../../formats/html.xsl" />
<xsl:include href="../../formats/language-elements-html.xsl" />

<xsl:template match="/book">
  <html>
    <head>
      <title><xsl:apply-templates select="title" /></title>
			<xsl:element name="link">
				<xsl:attribute name="rel">stylesheet</xsl:attribute>
				<xsl:attribute name="href"><xsl:value-of select="/book/base/@href" />shapes.css</xsl:attribute>
			</xsl:element>
    </head>
    <body>
			<xsl:call-template name="head-navigation" />
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
				<xsl:for-each select="/book/section/dynamic-variable[@identifier]">
					<xsl:sort select="@identifier" />
					<xsl:call-template name="name-to-linked-dynvar">
						<xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param>
					</xsl:call-template>
					  
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
					<xsl:for-each select="dynamic-variable[@identifier]">
						<xsl:sort select="@identifier" />
						<xsl:call-template name="name-to-linked-dynvar">
							<xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param>
						</xsl:call-template>
						  
					</xsl:for-each>
				</p>
				<xsl:apply-templates select="dynamic-variable" />
			</xsl:for-each>

		</body>
  </html>
</xsl:template>

</xsl:stylesheet>
