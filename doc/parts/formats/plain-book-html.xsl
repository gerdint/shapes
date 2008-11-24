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

    <xsl:for-each select="section">
			<hr class="thin"/>
			<xsl:choose>
				<xsl:when test="@id">
					<xsl:element name="a">
						<xsl:attribute name="name"><xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
						<h3><xsl:apply-templates select="title" /></h3>
					</xsl:element>
				</xsl:when>
				<xsl:otherwise>
					<h3><xsl:apply-templates select="title" /></h3>
				</xsl:otherwise>
			</xsl:choose>
      <xsl:apply-templates select="top" />
      <xsl:apply-templates select="body" />
      <xsl:for-each select="section">
				<xsl:choose>
					<xsl:when test="@id">
						<xsl:element name="a">
							<xsl:attribute name="name"><xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
							<h4><xsl:apply-templates select="title" /></h4>
						</xsl:element>
					</xsl:when>
					<xsl:otherwise>
						<h4><xsl:apply-templates select="title" /></h4>
					</xsl:otherwise>
				</xsl:choose>
				<xsl:apply-templates select="top" />
				<xsl:apply-templates select="body" />
				<xsl:for-each select="section">
					<xsl:choose>
						<xsl:when test="@id">
							<xsl:element name="a">
								<xsl:attribute name="name"><xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template></xsl:attribute>
								<h5><xsl:apply-templates select="title" /></h5>
							</xsl:element>
						</xsl:when>
						<xsl:otherwise>
							<h5><xsl:apply-templates select="title" /></h5>
						</xsl:otherwise>
					</xsl:choose>
					<xsl:apply-templates select="top" />
					<xsl:apply-templates select="body" />
				</xsl:for-each>
      </xsl:for-each>
    </xsl:for-each>
		</body>
  </html><xsl:text>
	</xsl:text>
</xsl:template>


</xsl:stylesheet>
