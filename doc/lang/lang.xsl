<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/book">
  <html>
    <head>
      <title><xsl:value-of select="title" /></title>
      <link rel="stylesheet" href="../shapes.css" />
    </head>
    <body>
    <h0><xsl:value-of select="title" /></h0>
    <hr class="thick"/>
    <xsl:apply-templates select="top" />
    <xsl:for-each select="section[@id='chap-kerntypes']">
      <h1><xsl:value-of select="title" /></h1>
      <xsl:apply-templates select="top" />
      <xsl:apply-templates select="body" />
      <xsl:for-each select="section">
	<h2><xsl:value-of select="title" /></h2>
	<xsl:apply-templates select="top" />
	<xsl:apply-templates select="body" />
	<xsl:for-each select="section">
	  <h3><xsl:value-of select="title" /></h3>
	  <xsl:apply-templates select="top" />
	  <xsl:apply-templates select="body" />
<!--	  <xsl:value-of select="content" disable-output-escaping="yes" /> -->
	</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
  </body>
  </html>
</xsl:template>

<xsl:template match="secref[@id]">
  <xsl:element name="a">
    <xsl:attribute name="href">#sec-<xsl:value-of select="@id" /></xsl:attribute>
    <xsl:apply-templates />
  </xsl:element>
</xsl:template>
<xsl:template match="secref/title">
  <xsl:variable name="dstid">
    <xsl:value-of select="../@id" />
  </xsl:variable>
  <xsl:apply-templates select="//section[@id=$dstid]/title" />
</xsl:template>

<xsl:template match="p">
  <p><xsl:apply-templates/></p>
</xsl:template>
<xsl:template match="ol">
  <ol><xsl:apply-templates/></ol>
</xsl:template>
<xsl:template match="ol/li">
  <li><xsl:apply-templates/></li>
</xsl:template>

<xsl:template match="pre">
<pre>
<xsl:apply-templates/>
</pre>
</xsl:template>
<xsl:template match="pre[@class]">
<xsl:element name="pre">
<xsl:attribute name="class"><xsl:value-of select="@class" /></xsl:attribute>
<xsl:apply-templates/>
</xsl:element>
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
<xsl:template match="typename[@class='replacable']">
  <typename class="replacable"><xsl:apply-templates/></typename>
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
<xsl:template match="a[@href]">
  <xsl:element name="a">
    <xsl:attribute name="href"><xsl:value-of select="@href" /></xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>
<xsl:template match="exampleswitch[@onclick]">
  <xsl:element name="div">
    <xsl:attribute name="style">display:inline;</xsl:attribute>
    <xsl:attribute name="onclick"><xsl:value-of select="@onclick" /></xsl:attribute>
    <exampleswitch><xsl:apply-templates/></exampleswitch>
  </xsl:element>    
</xsl:template>

<xsl:template match="syntax-table">
  <table cellspacing="5"><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="token-example-table">
  <table cellspacing="5">
    <xsl:apply-templates select="head"/>
    <xsl:apply-templates select="body"/>
  </table>
</xsl:template>
<xsl:template match="tight-table">
  <table><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="loose-table">
  <table cellspacing="5"><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="example-with-output[@*]">
  <table class="codefile">
    <tr><td><hr class="thick"/></td></tr>
    <tr><th colspan="3"><xsl:value-of select="@title" /></th></tr>
    <xsl:apply-templates />
    <tr><td><hr class="thick"/></td></tr>
  </table>
</xsl:template>
<xsl:template match="example-with-output/image[@*]">
  <tr align="center"><td>
      <xsl:element name="a">
	<xsl:attribute name="href"><xsl:value-of select="@pdf" /></xsl:attribute>
	<xsl:element name="img">
	  <xsl:attribute name="src"><xsl:value-of select="@jpg" /></xsl:attribute>
	  <xsl:attribute name="alt">Angry</xsl:attribute>
	</xsl:element>
      </xsl:element>
  </td></tr>
</xsl:template>
<xsl:template match="example-with-output/source[@file]">
  <tr><td><hr /></td></tr>
  <tr align="center"><td>Source: 
      <xsl:element name="exampleswitch">
	<xsl:attribute name="onclick">document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-src&apos;).style.display='inline'</xsl:attribute>
	show
      </xsl:element>
      — 
      <xsl:element name="exampleswitch">
	<xsl:attribute name="onclick">document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-src&apos;).style.display='none'</xsl:attribute>
	hide
      </xsl:element>
      — 
      <xsl:element name="a">
	<xsl:attribute name="href"><xsl:value-of select="@file" /></xsl:attribute>
	visit
      </xsl:element>
  </td></tr>
  <xsl:element name="tr">
    <xsl:attribute name="id"><xsl:value-of select="../@internal-id" />-src</xsl:attribute>
    <xsl:attribute name="style">display:none;</xsl:attribute>
    <td>
      <pre>
	<xsl:apply-templates/>
      </pre>
    </td>
  </xsl:element>
</xsl:template>
<xsl:template match="example-with-output/stdout">
  <tr><td><hr /></td></tr>
  <tr align="center"><td>stdout: 
      <xsl:element name="exampleswitch">
	<xsl:attribute name="onclick">document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-stdout&apos;).style.display='inline'</xsl:attribute>
	show
      </xsl:element>
      — 
      <xsl:element name="exampleswitch">
	<xsl:attribute name="onclick">document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-stdout&apos;).style.display='none'</xsl:attribute>
	hide
      </xsl:element>
  </td></tr>
  <xsl:element name="tr">
    <xsl:attribute name="id"><xsl:value-of select="../@internal-id" />-stdout</xsl:attribute>
    <xsl:attribute name="style">display:none;</xsl:attribute>
    <td>
      <pre>
	<xsl:apply-templates/>
      </pre>
    </td>
  </xsl:element>
</xsl:template>
<xsl:template match="tr">
  <tr><xsl:apply-templates/></tr>
</xsl:template>
<xsl:template match="token-example-table/head/tr">
  <tr align="left"><xsl:apply-templates/></tr>
</xsl:template>
<xsl:template match="code-file-table/tr">
  <tr align="center"><xsl:apply-templates/></tr>
</xsl:template>
<xsl:template match="tr[@align]">
  <xsl:element name="tr">
    <xsl:attribute name="align"><xsl:value-of select="./@align" /></xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
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
<xsl:template match="th[@colspan]">
  <xsl:element name="th">
    <xsl:attribute name="colspan"><xsl:value-of select="./@colspan" /></xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>

<xsl:template match="img[@*]">
  <xsl:element name="img">
    <xsl:attribute name="src"><xsl:value-of select="@src" /></xsl:attribute>
    <xsl:attribute name="alt"><xsl:value-of select="@alt" /></xsl:attribute>
  </xsl:element>
</xsl:template>

</xsl:stylesheet>

