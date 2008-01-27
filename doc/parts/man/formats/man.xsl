<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
xmlns:fn="http://www.w3.org/2005/02/xpath-functions">
<xsl:output method="text" indent="no" />

<xsl:include href="../../formats/man.xsl" />

<xsl:template match="/man">
<xsl:apply-templates select="manhead" />
<xsl:text>
.ensure-newline.TP
.B Note:
Due to limitations in the character encoding used for this page, some characters in the examples have been replaced by legacy substitutions.  These issues are not present in the
.SM HTML
version of this page.</xsl:text>
<xsl:for-each select="section"><xsl:text>
.ensure-line-break.SH </xsl:text><xsl:value-of select="upper-case(title)" /><xsl:text>
.ensure-line-break</xsl:text>
<xsl:apply-templates select="top/*" />
<xsl:apply-templates select="body/*" />
<xsl:for-each select="section"><xsl:text>
.ensure-line-break.SS </xsl:text><xsl:value-of select="title" /><xsl:text>
.ensure-line-break</xsl:text>
<xsl:apply-templates select="top/*" />
<xsl:apply-templates select="body/*" />
</xsl:for-each>
</xsl:for-each>
</xsl:template>

<xsl:template match="manhead">.TH <xsl:value-of select="prog-name" /> <xsl:value-of select="man-section" /> &quot;<xsl:value-of select="modification-date" />&quot; &quot;<xsl:value-of select="left-footer" />&quot; &quot;<xsl:value-of select="center-header" />&quot;</xsl:template>

<xsl:template match="synopsis-table">
<xsl:for-each select="synopsis-case"><xsl:apply-templates/><xsl:text>
</xsl:text></xsl:for-each>
</xsl:template>

<xsl:template match="synopsis-case"><xsl:apply-templates/></xsl:template>

<xsl:template match="see-also-items">
  <xsl:for-each select="see-also"><xsl:apply-templates/></xsl:for-each>
</xsl:template>
<xsl:template match="manpage[@tool,@section]"><xsl:text>.BR </xsl:text>&quot;<xsl:value-of select="@tool" />&quot; &quot;(<xsl:value-of select="@section" />)&quot;<xsl:text>
.ensure-line-break</xsl:text>
</xsl:template>

<xsl:template match="env-variable-list">
<apply-templates />
</xsl:template>
<xsl:template match="env-variable-item">
<xsl:text>

.B </xsl:text><xsl:value-of select="name" /><xsl:text>
.ensure-line-break</xsl:text>
<xsl:apply-templates select="description/*"/>
</xsl:template>

<xsl:template match="command-line-option-list">
<xsl:apply-templates />
</xsl:template>
<xsl:template match="command-line-item">
<xsl:text>

</xsl:text><xsl:apply-templates select="parameters" />
<xsl:apply-templates select="short-parameter" />
<xsl:apply-templates select="description/*"/>
</xsl:template>

<xsl:template match="command-line-item/parameters[@flag]"><xsl:text>
.ensure-line-break.B "</xsl:text><xsl:value-of select="@flag" /><xsl:text>"
.ensure-line-break</xsl:text><xsl:apply-templates /><xsl:text>
.ensure-line-break.br
.ensure-line-break</xsl:text></xsl:template>

<xsl:template match="command-line-item/short-parameter[@flag]"><xsl:text>
.ensure-line-break.BI &quot;</xsl:text><xsl:value-of select="@flag" /><xsl:text>&quot; &quot;</xsl:text><xsl:value-of select="."/><xsl:text>
.ensure-line-break.br
.ensure-line-break</xsl:text></xsl:template>

<xsl:template match="prog-name[@class='other']"><xsl:value-of select="." /></xsl:template>
<xsl:template match="prog-name"><xsl:text>
.ensure-line-break.B </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>

<xsl:template match="synopsis-case/syntax-name"><xsl:text>
.ensure-line-break.B </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>
<xsl:template match="synopsis-case/bnf"><xsl:text>
.ensure-line-break </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>
<xsl:template match="p/syntax-name"><xsl:text>
.ensure-line-break.B </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>
<xsl:template match="synopsis-case/replacable"><xsl:text>
.ensure-line-break.I "</xsl:text><xsl:value-of select="." /><xsl:text>"
.ensure-line-break</xsl:text></xsl:template>
<xsl:template match="parameters/replacable"><xsl:text>
.ensure-line-break.I </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>

<xsl:template match="p/em"><xsl:text>
.ensure-line-break.I </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>
<xsl:template match="p/b"><xsl:text>
.ensure-line-break.B </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>

</xsl:stylesheet>
