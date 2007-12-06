<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/man">
  <xsl:apply-templates select="manhead" />
  <xsl:for-each select="section">
<!--    .SH <xsl:value-of select="fn::upper-case(title)" /> -->
    .SH <xsl:value-of select="title" />
    <xsl:apply-templates select="top" />
    <xsl:apply-templates select="body" />
    <xsl:for-each select="section">
      .SS <xsl:value-of select="title" />
      <xsl:apply-templates select="top" />
      <xsl:apply-templates select="body" />
    </xsl:for-each>
  </xsl:for-each>
</xsl:template>

<xsl:template match="manhead">
  <xsl:value-of select="prog-name" />
  <xsl:value-of select="man-section" />
  &quot;<xsl:value-of select="modification-date" />&quot;
  &quot;<xsl:value-of select="left-footer" />&quot;
  &quot;<xsl:value-of select="center-header" />&quot;
</xsl:template>

<xsl:template match="p">
  <xsl:apply-templates/>
</xsl:template>
<xsl:template match="pre">
.br
<xsl:apply-templates/>
</xsl:template>

<xsl:template match="synopsis-table">
  <xsl:for-each select="synopsis-case">
		<xsl:apply-templates/>
	</xsl:for-each>
</xsl:template>

<xsl:template match="synopsis-case">
	<xsl:apply-templates/>
</xsl:template>

<xsl:template match="see-also-items">
  <xsl:for-each select="see-also">
.BR <xsl:apply-templates/>
	</xsl:for-each>
</xsl:template>

<xsl:template match="env-variable-description">
.B <xsl:value-of select="name" />
<xsl:apply-templates select="description"/>
</xsl:template>


<xsl:template match="prog-name[@class='other']"><xsl:value-of select="." /></xsl:template>
<xsl:template match="prog-name">
.B <xsl:value-of select="." />
</xsl:template>
<xsl:template match="file-name">
\e<xsl:value-of select="." />
</xsl:template>

<xsl:template match="synopsis-case/stx">
.B <xsl:value-of select="." />
</xsl:template>
<xsl:template match="synopsis-case/replacable">
.I <xsl:value-of select="." />
</xsl:template>

<xsl:template match="str-PDF">PDF</xsl:template>
<xsl:template match="str-Shapes">Shapes</xsl:template>
<xsl:template match="em-dash">\-</xsl:template>

</xsl:stylesheet>
