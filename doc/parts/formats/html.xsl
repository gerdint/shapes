<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="str-PDF"><span class="medium-caps">pdf</span></xsl:template>
<xsl:template match="str-Shapes">Shapes</xsl:template>
<xsl:template match="str-TeX"><span class="tex">T<sub>e</sub>X</span></xsl:template>
<xsl:template match="str-LaTeX"><span class="latex">L<sup>a</sup>T<sub>e</sub>X</span></xsl:template>
<xsl:template match="str-UTF-8"><span class="medium-caps">utf</span>-8</xsl:template>
<xsl:template match="str-2D">2<span class="medium-caps"><sup>d</sup></span></xsl:template>
<xsl:template match="str-3D">3<span class="medium-caps"><sup>d</sup></span></xsl:template>

<xsl:template match="char-cdot">*</xsl:template>
<xsl:template match="char-bullet">•</xsl:template>
<xsl:template match="char-str-open">`</xsl:template>
<xsl:template match="char-str-close">´</xsl:template>

<xsl:template match="physical"><span class="nowrap"><xsl:apply-templates select="scalar" /><span class="xx-small"> </span><xsl:apply-templates select="unit" /></span></xsl:template>
<xsl:template match="sci-fmt[@mantissa,@exp]"><span class="nowrap"><xsl:value-of select="@mantissa" /><span class="small-caps">e</span><xsl:value-of select="@exp" /></span></xsl:template>
<xsl:template match="quote">“<xsl:apply-templates />”</xsl:template>

<xsl:template match="filename"><filename><xsl:value-of select="." /></filename></xsl:template>

<xsl:template match="p">
  <p><xsl:apply-templates/></p>
</xsl:template>
<xsl:template match="ol">
  <ol><xsl:apply-templates/></ol>
</xsl:template>
<xsl:template match="ol/li">
  <li><xsl:apply-templates/></li>
</xsl:template>

<xsl:template match="pre">
<pre>
<xsl:apply-templates/>
</pre>
</xsl:template>
<xsl:template match="pre[@class]">
<xsl:element name="pre">
<xsl:attribute name="class"><xsl:value-of select="@class" /></xsl:attribute>
<xsl:apply-templates/>
</xsl:element>
</xsl:template>

<xsl:template match="note">
  <table class="note">
    <tr><td><xsl:apply-templates/></td></tr>
  </table>
</xsl:template>

<xsl:template match="binding[@name]">
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href">bindings.html#<xsl:value-of select="@name" /></xsl:attribute>
		<varname><xsl:value-of select="@name" /></varname>
	</xsl:element>
</xsl:template>
<xsl:template match="dynvar[@name]">
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href">dynvars.html#<xsl:value-of select="@name" /></xsl:attribute>
		<varname>@<xsl:value-of select="@name" /></varname>
	</xsl:element>
</xsl:template>
<xsl:template match="named-type[@name and not(@link)]">
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href">types.html#<xsl:value-of select="@name" /></xsl:attribute>
		<typename>§<xsl:value-of select="@name" /></typename>
	</xsl:element>
</xsl:template>
<xsl:template match="named-type[@name and @link='no']">
	<typename>§<xsl:value-of select="@name" /></typename>
</xsl:template>
<xsl:template match="named-state-type[@name and not(@link)]">
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href">states.html#<xsl:value-of select="@name" /></xsl:attribute>
		<typename>§•<xsl:value-of select="@name" /></typename>
	</xsl:element>
</xsl:template>
<xsl:template match="named-state-type[@name and @link='no']">
	<typename>§•<xsl:value-of select="@name" /></typename>
</xsl:template>
<xsl:template match="tol-param[@name and @link='no']">
	<tolparam><xsl:value-of select="@name" /></tolparam>
</xsl:template>
<xsl:template match="tol-param[@name and not(@link)]">
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href">algo-tol.html#tol-<xsl:value-of select="@name" /></xsl:attribute>
		<tolparam><xsl:value-of select="@name" /></tolparam>
	</xsl:element>
</xsl:template>


<xsl:template match="inline"><inline><xsl:apply-templates/></inline></xsl:template>
<xsl:template match="em"><em><xsl:apply-templates/></em></xsl:template>
<xsl:template match="bnf"><bnf><xsl:apply-templates/></bnf></xsl:template>
<xsl:template match="union-type">
	<typename><xsl:text>( </xsl:text></typename>
	<xsl:for-each select="./*">
		<xsl:if test="position() > 1">
			<typename><xsl:text>| </xsl:text></typename>
		</xsl:if>
		<xsl:apply-templates select="." />
		<xsl:text> </xsl:text>
	</xsl:for-each>
	<typename>)</typename>
</xsl:template>
<xsl:template match="structure-type">
	<typename><xsl:text>(&gt; </xsl:text></typename>
	<xsl:for-each select="field">
		<xsl:if test="@name">
			<xsl:value-of select="@name" />::
		</xsl:if>
		<xsl:if test="not(@name)">
			<xsl:value-of select="position( )" />::
		</xsl:if>
		<xsl:if test="not(type)">
			<typename>Any-Type</typename>
		</xsl:if>
		<xsl:apply-templates select="type" />
		<xsl:text> </xsl:text>
	</xsl:for-each>
	<typename>&lt;)</typename>
</xsl:template>
<xsl:template match="structure-type/field">
</xsl:template>
<xsl:template match="function-type">
	<typename><xsl:text>( </xsl:text></typename>
	<xsl:for-each select="arguments/arg">
		<xsl:if test="position() > 1">
			<typename><xsl:text>| </xsl:text></typename>
		</xsl:if>
		<xsl:apply-templates select="." />
		<xsl:text> </xsl:text>
	</xsl:for-each>
	<xsl:if test="arguments/sink[@name]">
		<xsl:text>&lt;&gt;</xsl:text><varname><xsl:value-of select="@name" /></varname>
	</xsl:if>
	<xsl:text> → </xsl:text>
	<xsl:apply-templates select="result" />
	<typename><xsl:text> )</xsl:text></typename>
</xsl:template>
<xsl:template match="typename"><typename><xsl:apply-templates/></typename></xsl:template>
<xsl:template match="typename[@class='replacable']">
  <typename class="replacable"><xsl:apply-templates/></typename>
</xsl:template>
<xsl:template match="varname">
  <varname><xsl:apply-templates/></varname>
</xsl:template>
<xsl:template match="varname[@class='replacable']">
  <varname class="replacable"><xsl:apply-templates/></varname>
</xsl:template>
<xsl:template match="lexerregexp">
  <lexerregexp><xsl:apply-templates/></lexerregexp>
</xsl:template>
<xsl:template match="syntax[@name and @class='new']">
  <xsl:element name="a">
    <xsl:attribute name="name">#stx-<xsl:value-of select="@name" /></xsl:attribute>
    <syntaxname class="new"><xsl:value-of select="@name"/></syntaxname>
  </xsl:element>
</xsl:template>
<xsl:template match="syntax[@name and not(@class)]">
  <xsl:element name="a">
    <xsl:attribute name="class">discrete</xsl:attribute>
    <xsl:attribute name="href">#stx-<xsl:value-of select="@name" /></xsl:attribute>
    <syntaxname><xsl:value-of select="@name"/></syntaxname>
  </xsl:element>
</xsl:template>
<xsl:template match="a[@href]">
  <xsl:element name="a">
    <xsl:attribute name="href"><xsl:value-of select="@href" /></xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>
<xsl:template match="command-line">
<pre class="terminal">
<xsl:apply-templates/>
</pre>
</xsl:template>

<xsl:template match="tight-table">
  <table><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="loose-table">
  <table cellspacing="5"><xsl:apply-templates select="tr"/></table>
</xsl:template>

<xsl:template match="tr">
  <tr><xsl:apply-templates/></tr>
</xsl:template>
<xsl:template match="tr[@align]">
  <xsl:element name="tr">
    <xsl:attribute name="align"><xsl:value-of select="./@align" /></xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>
<xsl:template match="head/tr/td">
  <th><xsl:apply-templates/></th>
</xsl:template>
<xsl:template match="td">
  <td><xsl:apply-templates/></td>
</xsl:template>
<xsl:template match="th">
  <th><xsl:apply-templates/></th>
</xsl:template>
<xsl:template match="th[@colspan]">
  <xsl:element name="th">
    <xsl:attribute name="colspan"><xsl:value-of select="./@colspan" /></xsl:attribute>
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>

<xsl:template match="img[@*]">
  <xsl:element name="img">
    <xsl:attribute name="src"><xsl:value-of select="@src" /></xsl:attribute>
    <xsl:attribute name="alt"><xsl:value-of select="@alt" /></xsl:attribute>
  </xsl:element>
</xsl:template>

</xsl:stylesheet>
