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
					<xsl:attribute name="href">#<xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
						<xsl:value-of select="title" />
					</xsl:element>
				</xsl:for-each>
			</p>

			<hr class="thin"/>
			<p class="center"><b>Alphabetical list</b></p>
			<p class="center">
				<xsl:for-each select="/book/section/system-binding[@identifier]">
					<xsl:sort select="@identifier" />
					<xsl:call-template name="name-to-linked-binding">
						<xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param>
					</xsl:call-template>
					  
				</xsl:for-each>
			</p>
			<hr class="thin"/>

			<xsl:for-each select="section">
				<h2>
					<xsl:element name="a">
						<xsl:attribute name="name"><xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
						<xsl:value-of select="title" />
					</xsl:element>
				</h2>
				<xsl:apply-templates select="top" />
				<p class="center">
					<xsl:for-each select="system-binding[@identifier]">
						<xsl:sort select="@identifier" />
						<xsl:call-template name="name-to-linked-binding">
							<xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param>
						</xsl:call-template>
						  
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
			<xsl:call-template name="name-to-binding"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
		</xsl:element>
	</h3>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="system-binding[@identifier]/function">
 	<xsl:apply-templates select="top"/>
 	<xsl:apply-templates select="case"/>
 	<xsl:apply-templates select="body"/>
</xsl:template>

<xsl:template match="system-binding[@identifier]/function/case">
	<h4 class="plain">
		<b>Case</b>  
 		<xsl:apply-templates select="arguments"/>
		<xsl:text>→ </xsl:text>
		<xsl:choose>
			<xsl:when test="@constructor-of">
				<xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@constructor-of" /></xsl:with-param></xsl:call-template>
			</xsl:when>
			<xsl:when test="result">
				<xsl:apply-templates select="result/type" />
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="name-to-linked-type"><xsl:with-param name="name">Void</xsl:with-param></xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</h4>
 	<xsl:apply-templates select="dynamic-references"/>
 	<xsl:apply-templates select="description"/>
</xsl:template>

<xsl:template match="function/case/arguments/arg[@identifier]">
	<xsl:call-template name="name-to-argument"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
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

<xsl:template match="function/case/arguments/state[@identifier]">
	<xsl:call-template name="name-to-state-argument"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
	<xsl:apply-templates select="type"/>
	<xsl:text> </xsl:text>
</xsl:template>
<xsl:template match="function/case/arguments/state[not(@identifier)]">
	<xsl:apply-templates select="type"/>
	<xsl:text> </xsl:text>
</xsl:template>
<xsl:template match="function/case/arguments/state/type"><xsl:text>::</xsl:text><xsl:apply-templates /></xsl:template>

<xsl:template match="dynamic-references[not(dynvar | dynstate)]">
	<p><b>Dynamic references:</b><xsl:text> </xsl:text><em>none</em></p>
</xsl:template>
<xsl:template match="dynamic-references[dynvar | dynstate]">
	<p><b>Dynamic references:</b><xsl:text> </xsl:text>
		<xsl:apply-templates />
	</p>
</xsl:template>
<xsl:template match="dynamic-references/dynstate[@name='all']">
	<xsl:text>The entire dynamic state</xsl:text>
</xsl:template>

<xsl:template match="system-binding[@identifier]/hot">
	<p><b>Hot value</b></p>
	<p>Spawns states of type <xsl:apply-templates select="constructor-of" />.</p>
</xsl:template>

<xsl:template match="system-binding[@identifier]/simple-value">
	<p><b>Type:</b><xsl:text> </xsl:text><xsl:apply-templates select="type"/></p>
	<xsl:apply-templates select="description"/>
	<xsl:if test="type/named-type">
		<xsl:variable name="self"><xsl:value-of select="../@identifier" /></xsl:variable>
		<xsl:variable name="t"><xsl:value-of select="type/named-type/@name" /></xsl:variable>
 		<p>
			<b>Related by type:</b>
			<xsl:for-each select="//system-binding[@identifier!=$self]/simple-value/type/named-type[@name=$t]">
				<xsl:text>  </xsl:text><xsl:call-template name="name-to-linked-binding"><xsl:with-param name="name"><xsl:value-of select="../../../@identifier" /></xsl:with-param></xsl:call-template>
			</xsl:for-each>
		</p>
	</xsl:if>
	<xsl:apply-templates select="see-also"/>
</xsl:template>

<xsl:template match="system-binding//see-also">
 	<p>
		<b>See also:</b>
		<xsl:for-each select="./*">
			<xsl:text>  </xsl:text><xsl:apply-templates select="."/>
		</xsl:for-each>
	</p>
</xsl:template>


</xsl:stylesheet>
