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
<xsl:output method="xml" indent="no" />

<xsl:include href="../../formats/html.xsl" />
<xsl:include href="../../formats/examplecode-html.xsl" />
<xsl:include href="../../formats/plain-book-html.xsl" />

<xsl:template match="syntax-table">
  <table class="syntax"><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="token-example-table">
  <table class="tokens">
    <xsl:apply-templates select="head"/>
    <xsl:apply-templates select="body"/>
  </table>
</xsl:template>

<xsl:template match="token-example-table/head/tr">
  <tr align="left"><xsl:apply-templates/></tr>
</xsl:template>

</xsl:stylesheet>

