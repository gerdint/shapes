<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" indent="no" />

<xsl:include href="../../formats/html.xsl" />

<xsl:template match="/man">
  <html>
    <head>
      <title><xsl:value-of select="manhead/center-header" /></title>
			<xsl:element name="link">
				<xsl:attribute name="rel">stylesheet</xsl:attribute>
				<xsl:attribute name="href"><xsl:value-of select="/book/base/@href" />shapes.css</xsl:attribute>
			</xsl:element>
    </head>
    <body>
			<xsl:apply-templates select="manhead" />
			<hr class="thick"/>
			<xsl:for-each select="section">
				<h2><xsl:value-of select="title" /></h2>
				<xsl:apply-templates select="top/*" />
				<xsl:apply-templates select="body/*" />
				<xsl:for-each select="section">
					<h3><xsl:value-of select="title" /></h3>
					<xsl:apply-templates select="top/*" />
					<xsl:apply-templates select="body/*" />
				</xsl:for-each>
			</xsl:for-each>
		</body>
  </html>
</xsl:template>

<xsl:template match="manhead">
	<h1><xsl:value-of select="center-header" /></h1>
	<p>This page corresponds to the man page <b><xsl:value-of select="@prog-name" /></b>(<xsl:value-of select="@section" />), dated <xsl:value-of select="@modification-date" />.</p>
	<p><em><b>Note:</b> The examples here may be presented using characters that may not be available in the man page format.  Hence, the examples may be presented differently in the man page.</em></p>
</xsl:template>

<xsl:template match="synopsis-table">
<xsl:for-each select="synopsis-case">
	<p><xsl:apply-templates/></p>
</xsl:for-each>
</xsl:template>

<xsl:template match="see-also-items">
  <xsl:for-each select="see-also"><xsl:apply-templates/><xsl:text> </xsl:text></xsl:for-each>
</xsl:template>
<xsl:template match="manpage[@tool,@section]">
	<xsl:element name="a"><xsl:attribute name="href">http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=%22<xsl:value-of select="@tool" />(<xsl:value-of select="@section" />)%22</xsl:attribute><b><xsl:value-of select="@tool" /></b>(<xsl:value-of select="@section" />)</xsl:element>
</xsl:template>

<xsl:template match="env-variable-list">
<ul><xsl:apply-templates /></ul>
</xsl:template>
<xsl:template match="env-variable-item">
<li>
<b><xsl:value-of select="name" /></b>
<xsl:apply-templates select="description/*"/>
</li>
</xsl:template>

<xsl:template match="command-line-option-list">
<ul><xsl:apply-templates /></ul>
</xsl:template>
<xsl:template match="command-line-item">
<li>
<xsl:apply-templates select="parameters" />
<xsl:apply-templates select="nosep-parameter" />
<xsl:apply-templates select="description/*"/>
</li>
</xsl:template>
<xsl:template match="command-line-tolparam[@name]">
  <xsl:variable name="tolname">
    <xsl:value-of select="@name" />
  </xsl:variable>
	<xsl:apply-templates select="/man/external/tolerance-parameter[@name=$tolname]" />
</xsl:template>
<xsl:template match="command-line-tolparam-remaining">
	<xsl:for-each select="/man/external/tolerance-parameter">
		<xsl:variable name="tolname">
			<xsl:value-of select="@name" />
		</xsl:variable>
		<xsl:if test="not(//command-line-tolparam[@name=$tolname])">
			<xsl:apply-templates select="." />
		</xsl:if>
	</xsl:for-each>
</xsl:template>

<xsl:template match="command-line-item/parameters[@flag]">
	<b><xsl:value-of select="@flag" /></b> <xsl:apply-templates /><br />
</xsl:template>

<xsl:template match="command-line-item/nosep-parameter[@flag]">
	<b><xsl:value-of select="@flag" /></b><program-param class="replacable"><xsl:value-of select="."/></program-param><br />
</xsl:template>

<xsl:template match="tolerance-parameter[@name]">
	<li>
		<xsl:apply-templates select="command-line-option-derived" />
		<xsl:apply-templates select="command-line-option" />
		<xsl:apply-templates select="description/*"/>
		<p>Default value: <xsl:apply-templates select="default" /></p>
	</li>
</xsl:template>

<xsl:template match="tolerance-parameter/command-line-option[@flag]">
	<b><xsl:value-of select="@flag" /></b> <xsl:apply-templates /><br />
</xsl:template>
<xsl:template match="tolerance-parameter[@name]/command-line-option-derived">
	<b>--<xsl:value-of select="../@name" />=</b><program-param class="replacable"><xsl:value-of select="." /></program-param><br />
</xsl:template>


<xsl:template match="prog-name[@class='other']"><xsl:value-of select="." /></xsl:template>
<xsl:template match="prog-name"><b><xsl:value-of select="." /></b></xsl:template>

<xsl:template match="synopsis-case/replacable"><program-param class="replacable"><xsl:value-of select="." /></program-param></xsl:template>
<xsl:template match="parameters/replacable"><program-param class="replacable"><xsl:value-of select="." /></program-param></xsl:template>
<xsl:template match="command-line-option/replacable"><program-param class="replacable"><xsl:value-of select="." /></program-param></xsl:template>

</xsl:stylesheet>
