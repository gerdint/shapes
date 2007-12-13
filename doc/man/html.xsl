<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" indent="no" />

<xsl:template match="/man">
  <html>
    <head>
      <title><xsl:value-of select="manhead/center-header" /></title>
      <link rel="stylesheet" href="../../shapes.css" />
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
	<p>This page corresponds to the man page <b><xsl:value-of select="prog-name" /></b>(<xsl:value-of select="man-section" />), dated <xsl:value-of select="modification-date" />.</p>
</xsl:template>

<xsl:template match="p">
  <p><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="pre">
<pre>
<xsl:apply-templates/>
</pre>
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

<xsl:template match="env-variable-description">
<b><xsl:value-of select="name" /></b>
<xsl:apply-templates select="description/*"/>
</xsl:template>

<xsl:template match="command-line-option-description">
<xsl:apply-templates select="parameters" />
<xsl:apply-templates select="short-parameter" />
<xsl:apply-templates select="description/*"/>
</xsl:template>

<xsl:template match="command-line-option-description/parameters[@flag]">
	<b><xsl:value-of select="@flag" /></b> <xsl:apply-templates /><br />
</xsl:template>

<xsl:template match="command-line-option-description/short-parameter[@flag]">
	<b><xsl:value-of select="@flag" /></b><paramname class="replacable"><xsl:value-of select="."/></paramname><br />
</xsl:template>

<xsl:template match="prog-name[@class='other']"><xsl:value-of select="." /></xsl:template>
<xsl:template match="prog-name"><b><xsl:value-of select="." /></b></xsl:template>
<xsl:template match="filename"><filename><xsl:value-of select="." /></filename></xsl:template>

<xsl:template match="syntax-name"><syntax-name><xsl:value-of select="." /></syntax-name></xsl:template>
<xsl:template match="bnf"><bnf><xsl:value-of select="." /></bnf></xsl:template>
<xsl:template match="synopsis-case/replacable"><paramname class="replacable"><xsl:value-of select="." /></paramname></xsl:template>
<xsl:template match="parameters/replacable"><paramname class="replacable"><xsl:value-of select="." /></paramname></xsl:template>

<xsl:template match="p/em"><em><xsl:value-of select="." /></em></xsl:template>
<xsl:template match="p/b"><b><xsl:value-of select="." /></b></xsl:template>

<xsl:template match="str-PDF">PDF</xsl:template>
<xsl:template match="str-Shapes">Shapes</xsl:template>
<xsl:template match="em-dash">—</xsl:template>

<xsl:template match="char-bullet">•</xsl:template>
<xsl:template match="char-str-open">`</xsl:template>
<xsl:template match="char-str-close">´</xsl:template>

</xsl:stylesheet>
