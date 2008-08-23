<?xml version="1.0" encoding="UTF-8"?>
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

