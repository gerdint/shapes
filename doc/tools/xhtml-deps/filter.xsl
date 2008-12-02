<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" indent="no" />

<xsl:template match="/html">
	<xsl:apply-templates select="body//a[@href]" />
	<xsl:apply-templates select="body//img[@src]" />
</xsl:template>

<xsl:template match="/html/body//a[@href]">
	<xsl:if test="not(starts-with(@href,'http://') or starts-with(@href,'https://') or starts-with(@href,'mailto:') or ends-with(@href,'/'))">
		<xsl:value-of select="/html/head/base/@href" />
		<xsl:choose>
			<xsl:when test="contains(@href,'#')">
				<xsl:value-of select="replace(substring-before(@href,'#'),'%25','%')" />
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="replace(@href,'%25','%')" />
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text> </xsl:text>
	</xsl:if>
</xsl:template>

<xsl:template match="/html/body//img[@src]">
	<xsl:value-of select="replace(@src,'%25','%')" /><xsl:text> </xsl:text>
</xsl:template>

</xsl:stylesheet>
