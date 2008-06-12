<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" indent="no" />

<xsl:include href="../../formats/html.xsl" />
<xsl:include href="../../formats/examplecode-html.xsl" />
<xsl:include href="../../formats/plain-book-html.xsl" />

<xsl:template match="alphabetical-index">
	<p class="center"><b>Alphabetical index</b></p>
	<p class="center">
		<xsl:for-each select="/book//system-binding[@identifier]">
			<xsl:sort select="@identifier" />
			<xsl:call-template name="name-to-linked-binding">
				<xsl:with-param name="extension-href"><xsl:apply-templates select="/book/meta-selflink" /></xsl:with-param>
				<xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param>
			</xsl:call-template>
			  
		</xsl:for-each>
	</p>
	<p class="center">
		<xsl:for-each select="/book//dynamic-variable[@identifier]">
			<xsl:sort select="@identifier" />
			<xsl:call-template name="name-to-linked-dynvar">
				<xsl:with-param name="extension-href"><xsl:apply-templates select="/book/meta-selflink" /></xsl:with-param>
				<xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param>
			</xsl:call-template>
			  
		</xsl:for-each>
	</p>
	<hr class="thin"/>
	<xsl:if test="/book/needs/a[@extension]">
		<p>
			<b>Dependencies:</b>
			<xsl:for-each select="/book/needs/a[@extension]">
				  
				<xsl:apply-templates select="."/>
			</xsl:for-each>
		</p>
		<hr class="thin"/>
	</xsl:if>
</xsl:template>

<xsl:template match="index-of-books">
	<ul>
		<xsl:apply-templates select="/book/external/book | /book/external/man" />
	</ul>
</xsl:template>

<xsl:template match="external/book">
	<li>
		<xsl:element name="a">
			<xsl:attribute name="href">../<xsl:apply-templates select="meta-selflink" /></xsl:attribute>
			<b><xsl:apply-templates select="title" /></b>
		</xsl:element>:
		<xsl:apply-templates select="description" />
	</li>
</xsl:template>

<xsl:template match="system-binding[@identifier]">
	<h3>
		<xsl:element name="a">
			<xsl:attribute name="name">bind/<xsl:value-of select="@identifier" /></xsl:attribute>
			<xsl:call-template name="name-to-binding"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
		</xsl:element>
	</h3>
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="system-binding[@identifier]/function">
 	<xsl:apply-templates select="top"/>
 	<xsl:apply-templates select="case"/>
 	<xsl:apply-templates select="body"/>
</xsl:template>

<xsl:template match="system-binding[@identifier]/function/case">
	<h4 class="plain">
		<b>Case</b>  
 		<xsl:apply-templates select="arguments"/>
		<xsl:text>→ </xsl:text>
		<xsl:choose>
			<xsl:when test="@constructor-of">
				<xsl:call-template name="name-to-linked-type"><xsl:with-param name="name"><xsl:value-of select="@constructor-of" /></xsl:with-param></xsl:call-template>
			</xsl:when>
			<xsl:when test="result">
				<xsl:apply-templates select="result/type" />
			</xsl:when>
			<xsl:otherwise>
				<xsl:call-template name="name-to-linked-type"><xsl:with-param name="name">Void</xsl:with-param></xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</h4>
 	<xsl:apply-templates select="type-templates"/>
 	<xsl:apply-templates select="dynamic-references"/>
 	<xsl:apply-templates select="description"/>
</xsl:template>

<xsl:template match="type-templates">
	<table class="type-templates">
		<xsl:apply-templates />
	</table>
</xsl:template>
<xsl:template match="function/case/type-templates/template[@name]">
	<tr>
		<td align="right"><xsl:call-template name="name-to-template-type"><xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param></xsl:call-template>  </td>
		<td>
			<xsl:apply-templates select="description"/>
		</td>
	</tr>
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

<xsl:template match="dynamic-references[not(dynvar | dynstate)]">
	<p><b>Dynamic references:</b><xsl:text> </xsl:text><em>none</em></p>
</xsl:template>
<xsl:template match="dynamic-references[dynvar | dynstate]">
	<p><b>Dynamic references:</b><xsl:text> </xsl:text>
		<xsl:apply-templates />
	</p>
</xsl:template>
<xsl:template match="dynamic-references/dynstate[@name='all']">
	<xsl:text>The entire dynamic state</xsl:text>
</xsl:template>

<xsl:template match="system-binding[@identifier]/hot">
	<p><b>Hot value</b></p>
	<p>Spawns states of type <xsl:apply-templates select="constructor-of" />.</p>
</xsl:template>

<xsl:template match="system-binding[@identifier]/simple-value">
	<p><b>Type:</b><xsl:text> </xsl:text><xsl:apply-templates select="type"/></p>
	<xsl:apply-templates select="description"/>
	<xsl:if test="type/named-type">
		<xsl:variable name="self"><xsl:value-of select="../@identifier" /></xsl:variable>
		<xsl:variable name="t"><xsl:value-of select="type/named-type/@name" /></xsl:variable>
 		<p>
			<b>Related by type:</b>
			<xsl:for-each select="//system-binding[@identifier!=$self]/simple-value/type/named-type[@name=$t]">
				<xsl:text>  </xsl:text><xsl:call-template name="name-to-linked-binding"><xsl:with-param name="name"><xsl:value-of select="../../../@identifier" /></xsl:with-param></xsl:call-template>
			</xsl:for-each>
		</p>
	</xsl:if>
	<xsl:apply-templates select="see-also"/>
</xsl:template>

<xsl:template match="system-binding//see-also">
 	<p>
		<b>See also:</b>
		<xsl:for-each select="./*">
			<xsl:text>  </xsl:text><xsl:apply-templates select="."/>
		</xsl:for-each>
	</p>
</xsl:template>


<xsl:template match="dynamic-variable[@identifier]">
  <xsl:variable name="self">
    <xsl:value-of select="@identifier" />
  </xsl:variable>
	<h3>
		<xsl:element name="a">
			<xsl:attribute name="name">dyn/<xsl:value-of select="@identifier" /></xsl:attribute>
			<xsl:call-template name="name-to-dynvar"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
		</xsl:element>
	</h3>
	<p><b>Used by: </b>
		<i>Not implemented!</i>
	</p>
	<xsl:apply-templates select="type" />
	<xsl:apply-templates select="constraint" />
	<xsl:apply-templates select="description" />
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/type">
	<p>
		<b>Type: </b><xsl:apply-templates />
	</p>
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/constraint">
	<p>
		<b>Constraint: </b><xsl:apply-templates />
	</p>
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/description">
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/constraint/self">
	<xsl:call-template name="name-to-dynvar"><xsl:with-param name="name"><xsl:value-of select="../../@identifier" /></xsl:with-param></xsl:call-template>
</xsl:template>


</xsl:stylesheet>
