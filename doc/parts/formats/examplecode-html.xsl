<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

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
    <xsl:apply-templates select="image" />
    <xsl:apply-templates select="caption" />
    <xsl:apply-templates select="source" />
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
<xsl:template match="example-with-output/caption">
  <tr><td>
			<here-caption>
				<xsl:apply-templates />
			</here-caption>
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

</xsl:stylesheet>
