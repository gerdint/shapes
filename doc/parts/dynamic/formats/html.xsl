<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" indent="no" />

<xsl:include href="../../formats/html.xsl" />

<xsl:template match="/book">
  <html>
    <head>
      <title><xsl:value-of select="title" /></title>
      <link rel="stylesheet" href="../../styles/html/shapes.css" />
    </head>
    <body>
			<h1><xsl:value-of select="title" /></h1>
			<hr class="thick"/>
			<xsl:apply-templates select="top" />

			<p class="center">
				<xsl:for-each select="/book/section/system-binding/function/case/dynamic-references/dynvar[@name]">
					<xsl:sort select="@name" />
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@name" /></xsl:attribute>
						@<xsl:value-of select="@name" />
					</xsl:element>
					  
				</xsl:for-each>
			</p>

		</body>
  </html>
</xsl:template>

</xsl:stylesheet>
