<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="str-PDF"><span class="medium-caps">pdf</span></xsl:template>
<xsl:template match="str-Shapes">Shapes</xsl:template>
<xsl:template match="str-TeX"><span class="tex">T<sub>e</sub>X</span></xsl:template>
<xsl:template match="str-LaTeX"><span class="latex">L<sup>a</sup>T<sub>e</sub>X</span></xsl:template>
<xsl:template match="str-UTF-8"><span class="medium-caps">utf</span>-8</xsl:template>

<xsl:template match="char-em-dash">—</xsl:template>
<xsl:template match="char-cdot">*</xsl:template>
<xsl:template match="char-bullet">•</xsl:template>
<xsl:template match="char-str-open">`</xsl:template>
<xsl:template match="char-str-close">´</xsl:template>

<xsl:template match="physical"><span class="nowrap"><xsl:apply-templates select="scalar" /><span class="xx-small"> </span><xsl:apply-templates select="unit" /></span></xsl:template>
<xsl:template match="sci-fmt[@mantissa,@exp]"><span class="nowrap"><xsl:value-of select="@mantissa" /><span class="small-caps">e</span><xsl:value-of select="@exp" /></span></xsl:template>
<xsl:template match="quote">“<xsl:apply-templates />”</xsl:template>

<xsl:template match="filename"><filename><xsl:value-of select="." /></filename></xsl:template>

</xsl:stylesheet>
