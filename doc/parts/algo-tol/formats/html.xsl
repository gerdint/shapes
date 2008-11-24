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
			<p><b>Algorithms:</b>
				<xsl:for-each select="section">
					  
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@id" /></xsl:attribute>
						<xsl:apply-templates select="title" />
					</xsl:element>
				</xsl:for-each>
			</p>

			<hr class="thin"/>
			<p class="center"><b>Tolerance parameters</b></p>
			<xsl:for-each select="/book/tolerance-parameter[@name]">
				<xsl:sort select="@name" />
				<h5>
					<xsl:element name="a">
						<xsl:attribute name="name">tol-<xsl:value-of select="@name" /></xsl:attribute>
						<tolparam><xsl:value-of select="@name" /></tolparam>
					</xsl:element>
					<xsl:text> : </xsl:text><xsl:apply-templates select="default" /><xsl:text> :: </xsl:text><xsl:apply-templates select="type" />
				</h5>
				<xsl:apply-templates select="description" />
			</xsl:for-each>
			<hr class="thin"/>

			<xsl:for-each select="section">
				<h3>
					<xsl:element name="a">
						<xsl:attribute name="name"><xsl:value-of select="@id" /></xsl:attribute>
						<xsl:value-of select="title" />
					</xsl:element>
				</h3>
				<xsl:apply-templates select="top" />
				<xsl:apply-templates select="body" />
				<xsl:for-each select="section">
					<h4><xsl:apply-templates select="title" /></h4>
					<xsl:apply-templates select="top" />
					<xsl:apply-templates select="body" />
					<xsl:for-each select="section">
						<h5><xsl:apply-templates select="title" /></h5>
						<xsl:apply-templates select="top" />
						<xsl:apply-templates select="body" />
					</xsl:for-each>
				</xsl:for-each>
			</xsl:for-each>
		</body>
  </html>
</xsl:template>

<xsl:template match="system-binding[@identifier]">
	<h4>
		<xsl:element name="a">
			<xsl:attribute name="name"><xsl:value-of select="@identifier" /></xsl:attribute>
			<xsl:value-of select="@identifier" />
		</xsl:element>
	</h4>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="system-binding[@identifier]/function">
 	<xsl:apply-templates select="case"/>
</xsl:template>

<xsl:template match="system-binding[@identifier]/function/case[@constructor-of]">
	<h4 class="plain">
		<b>Case</b>  
 		<xsl:apply-templates select="arguments"/>→ <typename><xsl:value-of select="@constructor-of" /></typename>
	</h4>
 	<xsl:apply-templates select="dynamic-references"/>
 	<xsl:apply-templates select="description"/>
</xsl:template>

<xsl:template match="function/case/arguments/arg[@identifier]">
	<xsl:call-template name="name-to-argument"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
	<xsl:apply-templates select="default"/>
	<xsl:apply-templates select="type"/>
	<xsl:text> </xsl:text>
</xsl:template>
<xsl:template match="function/case/arguments/arg[not(@identifier)]">
	<xsl:apply-templates select="default"/>
	<xsl:apply-templates select="type"/>
	<xsl:text> </xsl:text>
</xsl:template>
<xsl:template match="function/case/arguments/arg/default">:<xsl:apply-templates /></xsl:template>
<xsl:template match="function/case/arguments/arg/type"><xsl:text>::</xsl:text><xsl:apply-templates /></xsl:template>

<xsl:template match="dynamic-references[not(dynvar)]">
	<p><b>Dynamic references:</b><xsl:text> </xsl:text><em>none</em></p>
</xsl:template>
<xsl:template match="dynamic-references[dynvar]">
	<p><b>Dynamic references:</b><xsl:text> </xsl:text>
		<xsl:apply-templates />
	</p>
</xsl:template>

<xsl:template match="system-binding[@identifier]/hot">
	<p><b>Hot value</b></p>
	<p>Spawns states of type <xsl:apply-templates select="constructor-of" />.</p>
</xsl:template>



</xsl:stylesheet>
