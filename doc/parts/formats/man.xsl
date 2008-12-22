<?xml version="1.0" encoding="UTF-8"?>

<!-- This file is part of Shapes.                                           -->
<!--                                                                        -->
<!-- Shapes is free software: you can redistribute it and/or modify         -->
<!-- it under the terms of the GNU General Public License as published by   -->
<!-- the Free Software Foundation, either version 3 of the License, or      -->
<!-- any later version.                                                     -->
<!--                                                                        -->
<!-- Shapes is distributed in the hope that it will be useful,              -->
<!-- but WITHOUT ANY WARRANTY; without even the implied warranty of         -->
<!-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the          -->
<!-- GNU General Public License for more details.                           -->
<!--                                                                        -->
<!-- You should have received a copy of the GNU General Public License      -->
<!-- along with Shapes.  If not, see <http://www.gnu.org/licenses/>.        -->
<!--                                                                        -->
<!-- Copyright 2008 Henrik Tidefelt                                         -->

<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="str-PDF">PDF</xsl:template>
<xsl:template match="str-HTML">HTML</xsl:template>
<xsl:template match="str-XML">XML</xsl:template>
<xsl:template match="str-SVG">SVG</xsl:template>
<xsl:template match="str-PostScript">PostScript</xsl:template>
<xsl:template match="str-Shapes">Shapes</xsl:template>
<xsl:template match="str-TeX">TeX</xsl:template>
<xsl:template match="str-LaTeX">LaTeX</xsl:template>
<xsl:template match="str-pdfLaTeX">pdfLaTeX</xsl:template>
<xsl:template match="str-MetaPost">MetaPost</xsl:template>
<xsl:template match="str-C-plus-plus">C++</xsl:template>
<xsl:template match="str-UTF-8">UTF-8</xsl:template>
<xsl:template match="str-2D">2D</xsl:template>
<xsl:template match="str-3D">3D</xsl:template>

<xsl:template match="char-cdot">*</xsl:template>
<xsl:template match="char-bullet">#</xsl:template>
<xsl:template match="char-str-open">("</xsl:template>
<xsl:template match="char-str-close">")</xsl:template>

<xsl:template match="physical"><xsl:apply-templates select="scalar" /><xsl:apply-templates select="unit" /></xsl:template>
<xsl:template match="sci-fmt[@mantissa,@exp]"><xsl:value-of select="@mantissa" />e<xsl:value-of select="@exp" /></xsl:template>
<xsl:template match="quote">"<xsl:apply-templates />"</xsl:template>

<xsl:template match="tol-param"><xsl:text>
.ensure-line-break.B </xsl:text><xsl:value-of select="@name" /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>

<xsl:template match="filename"><xsl:text>
.ensure-line-break.I </xsl:text><xsl:value-of select="." /><xsl:text>
.ensure-line-break</xsl:text></xsl:template>

<xsl:template match="p"><xsl:apply-templates/><xsl:text>

</xsl:text></xsl:template>

<xsl:template match="command-line"><xsl:text>
.br
.ensure-line-break</xsl:text><xsl:apply-templates/><xsl:text>

</xsl:text></xsl:template>

<xsl:template match="env-var[@name]"><xsl:value-of select="@name" /></xsl:template>

</xsl:stylesheet>
