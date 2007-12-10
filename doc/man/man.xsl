<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:fn="http://www.w3.org/2005/02/xpath-functions">

<xsl:template match="/man">
<xsl:text>
</xsl:text>
<xsl:apply-templates select="manhead" />
<xsl:for-each select="section"><xsl:text>.SH </xsl:text><xsl:value-of select="upper-case(title)" /><xsl:text>
</xsl:text>
<xsl:apply-templates select="top/*" />
<xsl:apply-templates select="body/*" />
<xsl:for-each select="section">.SS <xsl:value-of select="title" />
<xsl:apply-templates select="top/*" />
<xsl:apply-templates select="body/*" />
</xsl:for-each>
</xsl:for-each>
</xsl:template>

<xsl:template match="manhead">.TH <xsl:value-of select="prog-name" /> <xsl:value-of select="man-section" /> &quot;<xsl:value-of select="modification-date" />&quot; &quot;<xsl:value-of select="left-footer" />&quot; &quot;<xsl:value-of select="center-header" />&quot;
</xsl:template>

<xsl:template match="p"><xsl:apply-templates/><xsl:text>
</xsl:text></xsl:template>

<xsl:template match="pre"><xsl:text>
.br  </xsl:text><xsl:apply-templates/><xsl:text>

.br
</xsl:text></xsl:template>

<xsl:template match="synopsis-table">
<xsl:for-each select="synopsis-case"><xsl:apply-templates/><xsl:text>
</xsl:text></xsl:for-each>
</xsl:template>

<xsl:template match="synopsis-case">
xsl:apply-templates/>
</xsl:template>

<xsl:template match="see-also-items">
  <xsl:for-each select="see-also"><xsl:text>.BR </xsl:text><xsl:apply-templates/><xsl:text>
</xsl:text></xsl:for-each>
</xsl:template>

<xsl:template match="env-variable-description">
<xsl:text>

.B </xsl:text><xsl:value-of select="name" /><xsl:text>
</xsl:text>
<xsl:apply-templates select="description/*"/>
</xsl:template>

<xsl:template match="prog-name[@class='other']"><xsl:value-of select="." /></xsl:template>
<xsl:template match="prog-name"><xsl:text>
.B </xsl:text><xsl:value-of select="." /></xsl:template>
<xsl:template match="filename"><xsl:text>
.I </xsl:text><xsl:value-of select="." />
</xsl:template>

<xsl:template match="synopsis-case/stx"><xsl:text>.B </xsl:text><xsl:value-of select="." /></xsl:template>
<xsl:template match="synopsis-case/replacable"><xsl:text>.I </xsl:text><xsl:value-of select="." /></xsl:template>

<xsl:template match="str-PDF">PDF</xsl:template>
<xsl:template match="str-Shapes">Shapes</xsl:template>
<xsl:template match="em-dash">\-</xsl:template>

<xsl:template match="post-blank"><xsl:text>
</xsl:text></xsl:template>
<xsl:template match="post-tight"><xsl:text>
</xsl:text></xsl:template>

</xsl:stylesheet>
