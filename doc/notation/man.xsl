<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="str-PDF">PDF</xsl:template>
<xsl:template match="str-Shapes">Shapes</xsl:template>
<xsl:template match="str-TeX">TeX</xsl:template>
<xsl:template match="str-LaTeX">LaTeX</xsl:template>
<xsl:template match="str-UTF-8">UTF-8</xsl:template>

<xsl:template match="char-em-dash">\-</xsl:template>
<xsl:template match="char-cdot">*</xsl:template>
<xsl:template match="char-bullet">#</xsl:template>
<xsl:template match="char-str-open">("</xsl:template>
<xsl:template match="char-str-close">")</xsl:template>

<xsl:template match="physical"><xsl:apply-templates select="scalar" /><xsl:apply-templates select="unit" /></xsl:template>
<xsl:template match="sci-fmt[@mantissa,@exp]"><xsl:value-of select="@mantissa" />e<xsl:value-of select="@exp" /></xsl:template>
<xsl:template match="quote">"<xsl:apply-templates />"</xsl:template>

<xsl:template match="filename"><xsl:text>
.ensure-line-break.I </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>

</xsl:stylesheet>
