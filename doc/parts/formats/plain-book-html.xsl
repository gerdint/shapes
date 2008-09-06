<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

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
			<h1><xsl:apply-templates select="title" /></h1>
    <hr class="thick"/>
    <xsl:apply-templates select="top" />

		<p><b>Sections:</b>
			<xsl:for-each select="section">
				  
				<xsl:element name="a">
					<xsl:attribute name="href">#<xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
					<xsl:apply-templates select="title" />
				</xsl:element>
			</xsl:for-each>
		</p>

    <xsl:for-each select="section">
			<hr class="thin"/>
			<xsl:choose>
				<xsl:when test="@id">
					<xsl:element name="a">
						<xsl:attribute name="name"><xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
						<h2><xsl:apply-templates select="title" /></h2>
					</xsl:element>
				</xsl:when>
				<xsl:otherwise>
					<h2><xsl:apply-templates select="title" /></h2>
				</xsl:otherwise>
			</xsl:choose>
      <xsl:apply-templates select="top" />
      <xsl:apply-templates select="body" />
      <xsl:for-each select="section">
				<xsl:choose>
					<xsl:when test="@id">
						<xsl:element name="a">
							<xsl:attribute name="name"><xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
							<h3><xsl:apply-templates select="title" /></h3>
						</xsl:element>
					</xsl:when>
					<xsl:otherwise>
						<h3><xsl:apply-templates select="title" /></h3>
					</xsl:otherwise>
				</xsl:choose>
				<xsl:apply-templates select="top" />
				<xsl:apply-templates select="body" />
				<xsl:for-each select="section">
					<xsl:choose>
						<xsl:when test="@id">
							<xsl:element name="a">
								<xsl:attribute name="name"><xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
								<h4><xsl:apply-templates select="title" /></h4>
							</xsl:element>
						</xsl:when>
						<xsl:otherwise>
							<h4><xsl:apply-templates select="title" /></h4>
						</xsl:otherwise>
					</xsl:choose>
					<xsl:apply-templates select="top" />
					<xsl:apply-templates select="body" />
				</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
		</body>
  </html><xsl:text>
	</xsl:text>
</xsl:template>


</xsl:stylesheet>
