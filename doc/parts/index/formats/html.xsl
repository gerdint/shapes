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
<xsl:include href="../../syntax/formats/syntax-html.xsl" />

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
			<h1><xsl:apply-templates select="title" /></h1>
			<hr class="thick"/>
			<xsl:apply-templates select="top" />
			<xsl:for-each select="section">
				<h2><xsl:apply-templates select="title" /></h2>
				<xsl:apply-templates select="top" />
				<xsl:apply-templates select="body" />
				<xsl:for-each select="section">
					<h3><xsl:apply-templates select="title" /></h3>
					<xsl:apply-templates select="top" />
					<xsl:apply-templates select="body" />
					<xsl:for-each select="section">
						<h4><xsl:apply-templates select="title" /></h4>
						<xsl:apply-templates select="top" />
						<xsl:apply-templates select="body" />
					</xsl:for-each>
				</xsl:for-each>
			</xsl:for-each>
		</body>
  </html><xsl:text>
	</xsl:text>
</xsl:template>

<xsl:template match="index-of-books">
	<ul>
		<xsl:apply-templates select="/book/external/book | /book/external/man | /book/external/group" />
	</ul>
</xsl:template>

<xsl:template match="external//book">
	<li>
		<xsl:element name="a">
			<xsl:attribute name="href"><xsl:value-of select="/book/base/@href" /><xsl:apply-templates select="meta-selflink" /></xsl:attribute>
			<b><xsl:apply-templates select="title" /></b>
		</xsl:element>:
		<xsl:apply-templates select="description" />
	</li>
</xsl:template>

<xsl:template match="external//man">
	<li>
		<xsl:element name="a">
			<xsl:attribute name="href"><xsl:value-of select="/book/base/@href" /><xsl:apply-templates select="manhead/meta-selflink" /></xsl:attribute>
			<b><xsl:apply-templates select="manhead/center-header" /></b>
		</xsl:element>:
		<xsl:apply-templates select="manhead/description" />
	</li>
</xsl:template>

<xsl:template match="external//group">
	<li>
		<b><xsl:apply-templates select="title" /></b>
		<xsl:apply-templates select="description" />
		<ul>
			<xsl:apply-templates select="book | man | group" />
		</ul>
	</li>
</xsl:template>


</xsl:stylesheet>

