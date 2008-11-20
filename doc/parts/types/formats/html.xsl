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
			<h1><xsl:apply-templates select="title" /></h1>
			<hr class="thick"/>
			<xsl:apply-templates select="top" />
			<p><b>Sections:</b>
				<xsl:for-each select="section">
					  
					<xsl:element name="a">
						<xsl:attribute name="href">#<xsl:value-of select="@id" /></xsl:attribute>
						<xsl:apply-templates select="title" />
					</xsl:element>
				</xsl:for-each>
			</p>

			<hr class="thin"/>
			<p class="center"><b>Alphabetical list</b></p>
			<p class="center">
				<xsl:for-each select="/book/section/coretype[@name]">
					<xsl:sort select="@name" />
					<xsl:call-template name="name-to-linked-type">
						<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
					</xsl:call-template>
					  
				</xsl:for-each>
			</p>
			<hr class="thin"/>

			<xsl:for-each select="section">
				<h2>
					<xsl:element name="a">
						<xsl:attribute name="name"><xsl:value-of select="@id" /></xsl:attribute>
						<xsl:apply-templates select="title" />
					</xsl:element>
				</h2>
				<xsl:apply-templates select="top" />
				<p class="center">
					<xsl:for-each select="coretype[@name]">
						<xsl:sort select="@name" />
						<xsl:call-template name="name-to-linked-type">
							<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
						</xsl:call-template>
						  
					</xsl:for-each>
				</p>
				<xsl:apply-templates select="coretype" />
			</xsl:for-each>

		</body>
  </html>
</xsl:template>

<xsl:template match="coretype[@name and not(definition)]">
  <xsl:variable name="self">
    <xsl:value-of select="@name" />
  </xsl:variable>
	<div>
		<p>
			<xsl:element name="table">
				<xsl:attribute name="class">type</xsl:attribute>
				<tr>
					<th class="big" colspan="2">
						<xsl:element name="a">
							<xsl:attribute name="name">type/<xsl:value-of select="@name" /></xsl:attribute>
							<xsl:call-template name="name-to-type"><xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param></xsl:call-template>
						</xsl:element>
					</th>
				</tr>
				<xsl:apply-templates select="abstraction" />
				<tr>
					<th class="heading" colspan="2">Construction</th>
				</tr>
				<xsl:if test="construction/syntax">
					<tr>
						<th class="horiz">Syntax:</th>
						<td>
							<xsl:for-each select="construction/syntax">
								<xsl:text>  </xsl:text>
								<xsl:apply-templates select="." />
							</xsl:for-each>
						</td>
					</tr>
				</xsl:if>
				<xsl:if test="/book/external//operator-unary/case/result[@consider-constructor='yes' and named-type/@name=$self] | /book/external//operator-binary/case/result[@consider-constructor='yes' and named-type/@name=$self]">
					<tr>
						<th class="horiz">Operators:</th>
						<td>
							<xsl:for-each select="/book/external//operator-unary[@side='prefix']/case[result/@consider-constructor='yes' and result/named-type/@name=$self]">
								<xsl:text>  ( </xsl:text>
								<xsl:value-of select="../@op" />
								<xsl:text> </xsl:text>
								<xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@type" /></xsl:with-param></xsl:call-template>
								<xsl:text> )</xsl:text>
							</xsl:for-each>
							<xsl:for-each select="/book/external//operator-unary[@side='postfix']/case/result[result/@consider-constructor='yes' and result/named-type/@name=$self]">
								<xsl:text>  ( </xsl:text>
								<xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@type" /></xsl:with-param></xsl:call-template>
								<xsl:text> </xsl:text>
 								<xsl:value-of select="../@op" />
								<xsl:text> )</xsl:text>
							</xsl:for-each>
							<xsl:for-each select="/book/external//operator-binary/case[result/@consider-constructor='yes' and result/named-type/@name=$self]">
								<xsl:text>  ( </xsl:text>
								<xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@first-type" /></xsl:with-param></xsl:call-template>
								<xsl:text> </xsl:text>
 								<xsl:value-of select="../@op" />
								<xsl:text> </xsl:text>
								<xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@second-type" /></xsl:with-param></xsl:call-template>
								<xsl:text> )</xsl:text>
							</xsl:for-each>
						</td>
					</tr>
				</xsl:if>
				<xsl:if test="/book/external//system-binding/function/case[@constructor-of=$self]">
					<tr>
						<th class="horiz">See also:</th>
						<td>
							<xsl:for-each select="/book/external//system-binding/function/case[@constructor-of=$self]">
								<xsl:text>  </xsl:text>
								<xsl:call-template name="name-to-linked-binding"><xsl:with-param name="name"><xsl:value-of select="../../@identifier" /></xsl:with-param></xsl:call-template>
							</xsl:for-each>
						</td>
					</tr>
				</xsl:if>
				<tr>
					<th class="heading" colspan="2">Fields</th>
				</tr>
				<xsl:apply-templates select="fields" />
				<xsl:if test="not(fields)">
					<tr>
						<td colspan="2">
							<p>A value of type <xsl:call-template name="name-to-type"><xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param></xsl:call-template> has no fields.</p>
						</td>
					</tr>
				</xsl:if>
				<xsl:apply-templates select="description" />
				<tr>
					<th class="heading" colspan="2">Involved operators</th>
				</tr>
				<tr>
					<td colspan="2">
						<xsl:apply-templates select="operators" />
						<table class="operator-table">
							<tr><td colspan="5"><hr class="thick"/></td></tr>
							<tr> <th colspan="5">Unary prefix operators</th> </tr>
							<tr> <th></th> <th>Operator</th> <th>Type</th> <th>Result</th> <th>Description</th> </tr>
							<tr><td colspan="5"><hr /></td></tr>
							<xsl:apply-templates select="/book/external//operator-unary[@side='prefix']/case[@type=$self]" />
							<tr><td colspan="5"><hr class="thick"/></td></tr>
							<tr> <th colspan="5">Unary postfix operators</th> </tr>
							<tr> <th>Type</th> <th>Operator</th> <th></th> <th>Result</th> <th>Description</th> </tr>
							<tr><td colspan="5"><hr /></td></tr>
							<xsl:apply-templates select="/book/external//operator-unary[@side='postfix']/case[@type=$self]" />
							<tr><td colspan="5"><hr class="thick"/></td></tr>
							<tr> <th colspan="5">Binary operators</th> </tr>
							<tr> <th>Type</th> <th>Operator</th> <th>Type</th> <th>Result</th> <th>Description</th> </tr>
							<tr><td colspan="5"><hr /></td></tr>
							<xsl:apply-templates select="/book/external//operator-binary/case[@first-type=$self and @second-type=$self]" />
							<xsl:apply-templates select="/book/external//operator-binary/case[@first-type=$self and @second-type!=$self]" />
							<xsl:apply-templates select="/book/external//operator-binary/case[@first-type!=$self and @second-type=$self]" />
							<tr><td colspan="5"><hr class="thick"/></td></tr>
						</table>
					</td>
				</tr>
				<xsl:if test="see-also">
					<tr>
						<th class="horiz">See also:</th>
						<td>
							<xsl:for-each select="see-also/*">
								<xsl:text>  </xsl:text><xsl:apply-templates select="."/>
							</xsl:for-each>
						</td>
					</tr>
				</xsl:if>
			</xsl:element>
		</p>
	</div>
</xsl:template>

<xsl:template match="coretype[@name and definition]">
  <xsl:variable name="self">
    <xsl:value-of select="@name" />
  </xsl:variable>
	<div>
		<p>
			<xsl:element name="table">
				<xsl:attribute name="class">type</xsl:attribute>
				<tr>
					<th class="big" colspan="2">
						<xsl:element name="a">
							<xsl:attribute name="name"><xsl:value-of select="@name" /></xsl:attribute>
							<xsl:call-template name="name-to-type"><xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param></xsl:call-template>
						</xsl:element>
					</th>
				</tr>
				<xsl:apply-templates select="abstraction" />
				<tr>
					<th class="horiz">Defined as:</th>
					<td><xsl:apply-templates select="definition"/></td>
				</tr>
				<xsl:if test="see-also or /book/external//system-binding/function/case[@constructor-of=$self]">
					<tr>
						<th class="horiz">See also:</th>
						<td>
							<xsl:for-each select="see-also/*">
								<xsl:text>  </xsl:text><xsl:apply-templates select="."/>
							</xsl:for-each>
							<xsl:for-each select="/book/external//system-binding/function/case[@constructor-of=$self]">
								<xsl:text>  </xsl:text>
								<xsl:call-template name="name-to-linked-binding"><xsl:with-param name="name"><xsl:value-of select="../../@identifier" /></xsl:with-param></xsl:call-template>
							</xsl:for-each>
						</td>
					</tr>
				</xsl:if>
			</xsl:element>
		</p>
	</div>
</xsl:template>

<xsl:template match="coretype[@name]/abstraction">
	<tr>
		<th class="horiz">Abstraction:</th>
		<td><xsl:apply-templates /></td>
	</tr>
</xsl:template>

<xsl:template match="coretype[@name]/fields">
	<tr>
		<td colspan="2">
			<xsl:apply-templates select="top" />
			<xsl:apply-templates select="../type-templates" />
			<xsl:element name="table">
				<xsl:attribute name="class">
					<xsl:choose>
						<xsl:when test="type-method">wide</xsl:when>
						<xsl:otherwise>loose</xsl:otherwise>
					</xsl:choose>
				</xsl:attribute>
				<tr> <th>Field</th> <th>Type</th> <th>Description</th> </tr>
				<tr><td colspan="3"><hr /></td></tr>
				<xsl:apply-templates select="type-field | type-method"/>
			</xsl:element>
		</td>
	</tr>
</xsl:template>

<xsl:template match="coretype/fields/type-field">
  <tr>
    <td><varname><xsl:value-of select="@name" /></varname></td>
    <td><xsl:apply-templates select="type"/></td>
    <td><xsl:apply-templates select="description"/></td>
  </tr>
</xsl:template>

<xsl:template match="coretype[@name]/description">
	<tr>
		<th class="heading" colspan="2">Description</th>
	</tr>
	<tr>
		<td colspan="2"><xsl:apply-templates /></td>
	</tr>
</xsl:template>

<xsl:template match="coretype[@name]/abstraction/p/self">
	<xsl:call-template name="name-to-type">
		<xsl:with-param name="name"><xsl:value-of select="../../../@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="coretype[@name]/description/p/self">
	<xsl:call-template name="name-to-type">
		<xsl:with-param name="name"><xsl:value-of select="../../../@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template match="coretype/fields/type-method">
  <tr>
    <td><varname><xsl:value-of select="@name" /></varname></td>
    <td><i>method</i></td>
    <td>
			<table class="function">
				<xsl:apply-templates select="function"/>
			</table>
		</td>
  </tr>
</xsl:template>

<xsl:template match="operator-unary[@side='prefix']/case">
  <tr>
    <td align="center"></td>
    <td align="center"><xsl:value-of select="../@op" /></td>
    <td align="center"><xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@type" /></xsl:with-param></xsl:call-template></td>
    <td align="center"><xsl:apply-templates select="result" /></td>
    <td><xsl:apply-templates select="description" /></td>
  </tr>
</xsl:template>
<xsl:template match="operator-unary[@side='postfix']/case">
  <tr>
    <td align="center"><xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@type" /></xsl:with-param></xsl:call-template></td>
    <td align="center"><xsl:value-of select="../@op" /></td>
    <td align="center"></td>
    <td align="center"><xsl:apply-templates select="result" /></td>
    <td><xsl:apply-templates select="description" /></td>
  </tr>
</xsl:template>
<xsl:template match="operator-binary/case">
  <tr>
    <td align="center"><xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@first-type" /></xsl:with-param></xsl:call-template></td>
    <td align="center"><xsl:value-of select="../@op" /></td>
    <td align="center"><xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@second-type" /></xsl:with-param></xsl:call-template></td>
    <td align="center"><xsl:apply-templates select="result" /></td>
    <td><xsl:apply-templates select="description" /></td>
  </tr>
</xsl:template>

</xsl:stylesheet>
