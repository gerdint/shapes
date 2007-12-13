<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

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
				<h1><xsl:value-of select="title" /></h1>
				<xsl:apply-templates select="top/*" />
				<xsl:apply-templates select="body/*" />
				<xsl:for-each select="section">
					<h2><xsl:value-of select="title" /></h2>
					<xsl:apply-templates select="top/*" />
					<xsl:apply-templates select="body/*" />
				</xsl:for-each>
			</xsl:for-each>
		</body>
  </html>
</xsl:template>

<xsl:template match="manhead">
	<h0><xsl:value-of select="center-header" /></h0>
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
  <xsl:for-each select="see-also"><xsl:apply-templates/></xsl:for-each>
</xsl:template>
<xsl:template match="manpage[@tool,@section]">
	<b><xsl:value-of select="@tool" /></b>(<xsl:value-of select="@section" />)
</xsl:template>

<xsl:template match="env-variable-description">
<h3><xsl:value-of select="name" /></h3>
<xsl:apply-templates select="description/*"/>
</xsl:template>

<xsl:template match="command-line-option-description">
<h3><xsl:apply-templates select="parameters" /></h3>
<xsl:apply-templates select="short-parameter" />
<xsl:apply-templates select="description/*"/>
</xsl:template>

<xsl:template match="command-line-option-description/parameters[@flag]">
	<b><xsl:value-of select="@flag" /></b> <xsl:apply-templates /><br />
</xsl:template>

<xsl:template match="command-line-option-description/short-parameter[@flag]">
	<b><xsl:value-of select="@flag" /></b><xsl:value-of select="."/><br />
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

</xsl:stylesheet>
