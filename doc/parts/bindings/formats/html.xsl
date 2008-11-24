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
<xsl:include href="../../formats/language-elements-html.xsl" />

<xsl:template match="/book">
  <html>
    <head>
      <title><xsl:apply-templates select="title" /></title>
			<xsl:element name="link">
				<xsl:attribute name="rel">stylesheet</xsl:attribute>
				<xsl:attribute name="href"><xsl:value-of select="/book/base/@href" />shapes.css</xsl:attribute>
			</xsl:element>
    </head>
    <body>
			<xsl:call-template name="head-navigation" />
			<h2><xsl:apply-templates select="title" /></h2>
			<hr class="thick"/>
			<xsl:apply-templates select="top" />
			<p><b>Sections:</b>
				<xsl:for-each select="section">
					  
					<xsl:element name="a">
					<xsl:attribute name="href">#<xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
						<xsl:apply-templates select="title" />
					</xsl:element>
				</xsl:for-each>
			</p>

			<hr class="thin"/>
			<p class="center"><b>Alphabetical list</b></p>
			<p class="center">
				<xsl:for-each select="/book/section/system-binding[@identifier]">
					<xsl:sort select="@identifier" />
					<xsl:call-template name="name-to-linked-binding">
						<xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param>
					</xsl:call-template>
					  
				</xsl:for-each>
			</p>
			<hr class="thin"/>

			<xsl:for-each select="section">
				<h3>
					<xsl:element name="a">
						<xsl:attribute name="name"><xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
						<xsl:apply-templates select="title" />
					</xsl:element>
				</h3>
				<xsl:apply-templates select="top" />
				<p class="center">
					<xsl:for-each select="system-binding[@identifier]">
						<xsl:sort select="@identifier" />
						<xsl:call-template name="name-to-linked-binding">
							<xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param>
						</xsl:call-template>
						  
					</xsl:for-each>
				</p>
				<xsl:apply-templates select="system-binding" />
			</xsl:for-each>
		</body>
  </html>
</xsl:template>

</xsl:stylesheet>
