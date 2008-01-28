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
    <h0><xsl:value-of select="title" /></h0>
    <hr class="thick"/>
    <xsl:apply-templates select="top" />
    <xsl:for-each select="section">
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
  </html><xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="secref-title[@id]">
  <xsl:variable name="dstid">
    <xsl:value-of select="@id" />
  </xsl:variable>
  <xsl:element name="a">
    <xsl:attribute name="href">#sec-<xsl:value-of select="@id" /></xsl:attribute>
    <xsl:apply-templates select="//section[@id=$dstid]/title" />
  </xsl:element>
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

<xsl:template match="syntax-table">
  <table cellspacing="5"><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="token-example-table">
  <table cellspacing="5">
    <xsl:apply-templates select="head"/>
    <xsl:apply-templates select="body"/>
  </table>
</xsl:template>

<xsl:template match="exampleswitch[@onclick]">
  <xsl:element name="div">
    <xsl:attribute name="style">display:inline;</xsl:attribute>
    <xsl:attribute name="onclick"><xsl:value-of select="@onclick" /></xsl:attribute>
    <exampleswitch><xsl:apply-templates/></exampleswitch>
  </xsl:element>
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
	<xsl:attribute name="href">../../output/<xsl:value-of select="@pdf" /></xsl:attribute>
	<xsl:element name="img">
	  <xsl:attribute name="src">../../output/<xsl:value-of select="@jpg" /></xsl:attribute>
	  <xsl:attribute name="alt">Angry</xsl:attribute>
	</xsl:element>
      </xsl:element>
  </td></tr>
</xsl:template>
<xsl:template match="example-with-output/source[@file]">
  <tr><td><hr /></td></tr>
  <tr align="center"><td>Source: 
      <xsl:element name="exampleswitch">
				<xsl:attribute name="onclick">if(document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-src&apos;).style.display=='none'){document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-src&apos;).style.display='inline'}else{document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-src&apos;).style.display='none'}</xsl:attribute>
				show/hide
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
	<xsl:attribute name="onclick">if(document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-stdout&apos;).style.display=='none'){document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-stdout&apos;).style.display='inline'}else{document.getElementById(&apos;<xsl:value-of select="../@internal-id" />-stdout&apos;).style.display='none'}</xsl:attribute>
	show/hide
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
<xsl:template match="token-example-table/head/tr">
  <tr align="left"><xsl:apply-templates/></tr>
</xsl:template>
<xsl:template match="code-file-table/tr">
  <tr align="center"><xsl:apply-templates/></tr>
</xsl:template>

<xsl:template match="type-field-table">
  <table cellspacing="5">
    <tr><td colspan="3"><hr class="thick"/></td></tr>
    <tr> <th>Field</th> <th>Type</th> <th>Description</th> </tr>
    <tr><td colspan="3"><hr /></td></tr>
    <xsl:apply-templates select="field"/>
    <tr><td colspan="3"><hr class="thick"/></td></tr>
  </table>
</xsl:template>
<xsl:template match="type-field-table/field">
  <tr>
    <td><varname><xsl:value-of select="@name" /></varname></td>
    <td><xsl:apply-templates select="type"/></td>
    <td><xsl:apply-templates select="description"/></td>
  </tr>
</xsl:template>

<xsl:template match="operator-table[@type]">
  <xsl:variable name="t">
    <xsl:value-of select="@type" />
  </xsl:variable>
  <table class="operator-table">
    <tr><td colspan="5"><hr class="thick"/></td></tr>
    <tr> <th colspan="5">Unary prefix operators</th> </tr>
    <tr> <th></th> <th>Operator</th> <th>Type</th> <th>Result</th> <th>Description</th> </tr>
    <tr><td colspan="5"><hr /></td></tr>
    <xsl:apply-templates select="//operator-unary[@side='prefix']/case[@type=$t]" />
    <tr><td colspan="5"><hr class="thick"/></td></tr>
    <tr> <th colspan="5">Unary postfix operators</th> </tr>
    <tr> <th>Type</th> <th>Operator</th> <th></th> <th>Result</th> <th>Description</th> </tr>
    <tr><td colspan="5"><hr /></td></tr>
    <xsl:apply-templates select="//operator-unary[@side='postfix']/case[@type=$t]" />
    <tr><td colspan="5"><hr class="thick"/></td></tr>
    <tr> <th colspan="5">Binary operators</th> </tr>
    <tr> <th>Type</th> <th>Operator</th> <th>Type</th> <th>Result</th> <th>Description</th> </tr>
    <tr><td colspan="5"><hr /></td></tr>
    <xsl:apply-templates select="//operator-binary/case[@first-type=$t and @second-type=$t]" />
    <xsl:apply-templates select="//operator-binary/case[@first-type=$t and @second-type!=$t]" />
    <xsl:apply-templates select="//operator-binary/case[@first-type!=$t and @second-type=$t]" />
    <tr><td colspan="5"><hr class="thick"/></td></tr>
  </table>
</xsl:template>
<xsl:template match="operator-unary[@side='prefix']/case">
  <tr>
    <td align="center"></td>
    <td align="center"><xsl:value-of select="../@op" /></td>
    <td align="center"><typename><xsl:value-of select="@type" /></typename></td>
    <td align="center"><xsl:apply-templates select="result" /></td>
    <td><xsl:apply-templates select="description" /></td>
  </tr>
</xsl:template>
<xsl:template match="operator-unary[@side='postfix']/case">
  <tr>
    <td align="center"><typename><xsl:value-of select="@type" /></typename></td>
    <td align="center"><xsl:value-of select="../@op" /></td>
    <td align="center"></td>
    <td align="center"><xsl:apply-templates select="result" /></td>
    <td><xsl:apply-templates select="description" /></td>
  </tr>
</xsl:template>
<xsl:template match="operator-binary/case">
  <tr>
    <td align="center"><typename><xsl:value-of select="@first-type" /></typename></td>
    <td align="center"><xsl:value-of select="../@op" /></td>
    <td align="center"><typename><xsl:value-of select="@second-type" /></typename></td>
    <td align="center"><xsl:apply-templates select="result" /></td>
    <td><xsl:apply-templates select="description" /></td>
  </tr>
</xsl:template>

<xsl:template match="constructor-table[@type]">
  <xsl:variable name="t">
    <xsl:value-of select="@type" />
  </xsl:variable>
  <table class="constructor-table">
    <tr><td colspan="3"><hr class="thick"/></td></tr>
    <tr> <th colspan="3">Constructors</th> </tr>
    <tr> <th>Name</th> <th>Arguments</th> <th>Description</th> </tr>
    <tr><td colspan="3"><hr /></td></tr>
    <xsl:apply-templates select="//system-binding/function/case[@constructor-of=$t]" />
    <tr><td colspan="3"><hr class="thick"/></td></tr>
  </table>
</xsl:template>
<xsl:template match="//system-binding/function/case">
  <tr>
    <td align="center"><varname><xsl:value-of select="../../@identifier" /></varname>  </td>
    <td align="center"><xsl:apply-templates select="arguments/*" /></td>
    <td><xsl:apply-templates select="description" /></td>
  </tr>
</xsl:template>

<xsl:template match="function/case/arguments/arg[not(@identifier)]">(__)<xsl:apply-templates select="default" />::<xsl:apply-templates select="type" /><xsl:text> </xsl:text></xsl:template>
<xsl:template match="function/case/arguments/arg[@identifier]"><varname><xsl:value-of select="@identifier" /><xsl:apply-templates select="default" /></varname>::<xsl:apply-templates select="type" /><xsl:text> </xsl:text></xsl:template>
<xsl:template match="function/case/arguments/arg/default">:<xsl:apply-templates /></xsl:template>

</xsl:stylesheet>

