<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
  <html>
    <head>
      <title><xsl:value-of select="title" /></title>
      <link rel="stylesheet" href="../shapes.css" />
    </head>
    <body>
    <h0><xsl:value-of select="book/title" /></h0>
    <xsl:for-each select="book/chapter">
      <h1><xsl:value-of select="title" /></h1>
      <xsl:for-each select="section">
	<h2><xsl:value-of select="title" /></h2>
	<xsl:for-each select="subsection">
	  <h3><xsl:value-of select="title" /></h3>
<!--	  <xsl:value-of select="content" disable-output-escaping="yes" /> -->
	  <xsl:apply-templates select="content" />
	</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </body>
  </html>
</xsl:template>

<xsl:template match="p">
  <p><xsl:apply-templates/></p>
</xsl:template>

<xsl:template match="inline">
  <inline><xsl:apply-templates/></inline>
</xsl:template>
<xsl:template match="em">
  <em><xsl:apply-templates/></em>
</xsl:template>
<xsl:template match="bnf">
  <bnf><xsl:apply-templates/></bnf>
</xsl:template>
<xsl:template match="typename">
  <typename><xsl:apply-templates/></typename>
</xsl:template>
<xsl:template match="lexerregexp">
  <lexerregexp><xsl:apply-templates/></lexerregexp>
</xsl:template>
<xsl:template match="syntaxname[@class='new']">
  <xsl:element name="a">
    <xsl:attribute name="name">#stx-<xsl:value-of select="." /></xsl:attribute>
    <syntaxname class="new"><xsl:apply-templates/></syntaxname>
  </xsl:element>
</xsl:template>
<xsl:template match="syntaxname">
  <xsl:element name="a">
    <xsl:attribute name="href">#stx-<xsl:value-of select="." /></xsl:attribute>
    <syntaxname><xsl:apply-templates/></syntaxname>
  </xsl:element>
</xsl:template>

<xsl:template match="syntax-table">
  <table cellspacing="5"><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="token-example-table">
  <table cellspacing="5"><xsl:apply-templates select="head"/></table>
  <table cellspacing="5"><xsl:apply-templates select="body"/></table>
</xsl:template>
<xsl:template match="tr">
  <tr><xsl:apply-templates/></tr>
</xsl:template>
<xsl:template match="head/tr/td">
  <th><xsl:apply-templates/></th>
</xsl:template>
<xsl:template match="td">
  <td><xsl:apply-templates/></td>
</xsl:template>
<xsl:template match="th">
  <th><xsl:apply-templates/></th>
</xsl:template>

</xsl:stylesheet>

