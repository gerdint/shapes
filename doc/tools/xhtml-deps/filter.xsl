<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" indent="no" />

<xsl:template match="/html">
	<xsl:apply-templates select="body//a[@href]" />
	<xsl:apply-templates select="body//img[@src]" />
</xsl:template>

<xsl:template match="/html/body//a[@href]">
	<xsl:if test="not(starts-with(@href,'http://'))">
		<xsl:value-of select="/html/head/base/@href" />
		<xsl:choose>
			<xsl:when test="contains(@href,'#')">
				<xsl:value-of select="substring-before(@href,'#')" />
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="@href" />
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text> </xsl:text>
	</xsl:if>
</xsl:template>

<xsl:template match="/html/body//img[@src]">
	<xsl:value-of select="@src" /><xsl:text> </xsl:text>
</xsl:template>

</xsl:stylesheet>
