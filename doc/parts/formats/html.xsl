<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="str-PDF"><span class="medium-caps">pdf</span></xsl:template>
<xsl:template match="str-Shapes">Shapes</xsl:template>
<xsl:template match="str-TeX"><span class="tex">T<sub>e</sub>X</span></xsl:template>
<xsl:template match="str-LaTeX"><span class="latex">L<sup>a</sup>T<sub>e</sub>X</span></xsl:template>
<xsl:template match="str-pdfLaTeX"><span class="latex">pdfL<sup>a</sup>T<sub>e</sub>X</span></xsl:template>
<xsl:template match="str-MetaPost">MetaPost</xsl:template>
<xsl:template match="str-UTF-8"><span class="medium-caps">utf</span>-8</xsl:template>
<xsl:template match="str-2D">2<span class="medium-caps"><sup>d</sup></span></xsl:template>
<xsl:template match="str-3D">3<span class="medium-caps"><sup>d</sup></span></xsl:template>
<xsl:template name="str-Google" match="str-Google"><span class="Google"><blue>G</blue><red>o</red><yellow>o</yellow><blue>g</blue><green>l</green><red>e</red><black>™</black></span></xsl:template>

<xsl:template match="abbr-etc">&amp;c</xsl:template>

<xsl:template match="char-cdot">*</xsl:template>
<xsl:template match="char-bullet">•</xsl:template>
<xsl:template match="char-str-open">`</xsl:template>
<xsl:template match="char-str-close">´</xsl:template>

<xsl:template match="physical"><span class="nowrap"><xsl:apply-templates select="scalar" /><span class="xx-small"> </span><xsl:apply-templates select="unit" /></span></xsl:template>
<xsl:template match="sci-fmt[@mantissa,@exp]"><span class="nowrap"><xsl:value-of select="@mantissa" /><span class="small-caps">e</span><xsl:value-of select="@exp" /></span></xsl:template>
<xsl:template match="quote">“<xsl:apply-templates />”</xsl:template>

<xsl:template match="filename"><filename><xsl:value-of select="." /></filename></xsl:template>

<xsl:template match="p">
  <div class="p"><xsl:apply-templates/></div>
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

<xsl:template name="part-to-href">
	<xsl:param name="name" />
	<xsl:choose>
		<xsl:when test="$name='syntax'">syntax.html</xsl:when>
		<xsl:when test="$name='bindings'">bindings.html</xsl:when>
		<xsl:when test="$name='states'">states.html</xsl:when>
		<xsl:when test="$name='dynamic'">dynvars.html</xsl:when>
		<xsl:when test="$name='types'">types.html</xsl:when>
		<xsl:when test="$name='state-types'">state-types.html</xsl:when>
		<xsl:when test="$name='algo-tol'">algo-tol.html</xsl:when>
		<xsl:when test="$name='man'">man.html</xsl:when>
		<xsl:when test="$name='tutorial'">tutorial.html</xsl:when>
		<xsl:when test="$name='extensions'">extensions/index_.html</xsl:when>
	</xsl:choose>
</xsl:template>
<xsl:template name="extension-to-href">
	<xsl:param name="name" />
	<xsl:text>extensions/</xsl:text><xsl:value-of select="$name" /><xsl:text>.html</xsl:text>
</xsl:template>
<xsl:template name="id-to-anchor-name">
	<xsl:param name="id" />
	<xsl:text>secid:</xsl:text><xsl:value-of select="$id" />
</xsl:template>

<xsl:template match="part-href[@name]">
	<xsl:call-template name="part-to-href">
			<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="extension-href[@name]">
	<xsl:call-template name="extension-to-href">
			<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template match="a[@part | @id]">
	<xsl:element name="a">
		<xsl:attribute name="href">
			<xsl:if test="@part">
				<xsl:call-template name="part-to-href">
					<xsl:with-param name="name"><xsl:value-of select="@part" /></xsl:with-param>
				</xsl:call-template>
			</xsl:if>
			<xsl:if test="@id">
				<xsl:text>#</xsl:text>
				<xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template>
			</xsl:if>
		</xsl:attribute>
		<xsl:choose>
			<xsl:when test="node()">
				<xsl:apply-templates />
			</xsl:when>
			<xsl:when test="@part">
				<filename>
					<xsl:call-template name="part-to-href">
						<xsl:with-param name="name"><xsl:value-of select="@part" /></xsl:with-param>
					</xsl:call-template>
				</filename>
			</xsl:when>
			<xsl:otherwise>
				<xsl:variable name="dstid">
					<xsl:value-of select="@id" />
				</xsl:variable>
				<xsl:apply-templates select="//section[@id=$dstid]/title" />
			</xsl:otherwise>
		</xsl:choose>
	</xsl:element>
</xsl:template>
<xsl:template match="a[@id]/title">
  <xsl:variable name="dstid">
    <xsl:value-of select="../@id" />
  </xsl:variable>
  <xsl:apply-templates select="//section[@id=$dstid]/title" />
</xsl:template>
<xsl:template match="a[@extension]">
	<xsl:element name="a">
		<xsl:attribute name="href">
			<xsl:call-template name="extension-to-href">
				<xsl:with-param name="name"><xsl:value-of select="@extension" /></xsl:with-param>
			</xsl:call-template>
			<xsl:if test="@id">
				<xsl:text>#</xsl:text>
				<xsl:call-template name="id-to-anchor-name"><xsl:with-param name="id"><xsl:value-of select="@id" /></xsl:with-param></xsl:call-template>
			</xsl:if>
		</xsl:attribute>
		<filename><xsl:value-of select="@extension" /></filename>
	</xsl:element>
</xsl:template>

<xsl:template match='a[@method="google-lucky" and @query]'>
	<xsl:element name="a">
		<xsl:attribute name="href">http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=<xsl:value-of select="@query" /></xsl:attribute>
		<xsl:choose>
			<xsl:when test="node()">
				<xsl:apply-templates />
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>[</xsl:text><xsl:call-template name="str-Google" /><xsl:text>_lucky `</xsl:text><xsl:value-of select="@query" /><xsl:text>´]</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:element>
</xsl:template>
<xsl:template match='a[@method="google" and @query]'>
	<xsl:element name="a">
		<xsl:attribute name="href">http://www.google.com/search?q=<xsl:value-of select="@query" /></xsl:attribute>
		<xsl:choose>
			<xsl:when test="node()">
				<xsl:apply-templates />
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>[</xsl:text><xsl:call-template name="str-Google" /><xsl:text> `</xsl:text><xsl:value-of select="@query" /><xsl:text>´]</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:element>
</xsl:template>
<xsl:template match="a[@query]/query">
  <xsl:value-of select="../@query" />
</xsl:template>


<xsl:template name="name-to-binding">
	<xsl:param name="name" />
	<varname><xsl:value-of select="$name" /></varname>
</xsl:template>
<xsl:template name="name-to-linked-binding">
	<xsl:param name="extension" />
	<xsl:param name="extension-href" />
	<xsl:param name="name" />
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href">
			<xsl:choose>
				<xsl:when test="$extension">
					<xsl:call-template name="extension-to-href"><xsl:with-param name="name"><xsl:value-of select="$extension" /></xsl:with-param></xsl:call-template>#bind/<xsl:value-of select="$name" />
				</xsl:when>
				<xsl:when test="$extension-href">
					<xsl:value-of select="$extension-href" />#bind/<xsl:value-of select="$name" />
				</xsl:when>
				<xsl:otherwise>
					<xsl:call-template name="part-to-href"><xsl:with-param name="name">bindings</xsl:with-param></xsl:call-template>#<xsl:value-of select="$name" />
				</xsl:otherwise>
			</xsl:choose>
		</xsl:attribute>
		<xsl:call-template name="name-to-binding">
			<xsl:with-param name="name"><xsl:value-of select="$name" /></xsl:with-param>
		</xsl:call-template>
	</xsl:element>
</xsl:template>
<xsl:template match="binding[@name and not(@extension)]">
	<xsl:call-template name="name-to-linked-binding">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="binding[@name and @extension]">
	<xsl:call-template name="name-to-linked-binding">
		<xsl:with-param name="extension"><xsl:value-of select="@extension" /></xsl:with-param>
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="user-binding[@name]">
	<xsl:call-template name="name-to-binding">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template name="name-to-dynvar">
	<xsl:param name="name" />
	<varname>@<xsl:value-of select="$name" /></varname>
</xsl:template>
<xsl:template name="name-to-linked-dynvar">
	<xsl:param name="extension" />
	<xsl:param name="extension-href" />
	<xsl:param name="name" />
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href">
			<xsl:choose>
				<xsl:when test="$extension">
					<xsl:call-template name="extension-to-href"><xsl:with-param name="name"><xsl:value-of select="$extension" /></xsl:with-param></xsl:call-template>#dyn/<xsl:value-of select="$name" />
				</xsl:when>
				<xsl:when test="$extension-href">
					<xsl:value-of select="$extension-href" />#dyn/<xsl:value-of select="$name" />
				</xsl:when>
				<xsl:otherwise>
					<xsl:call-template name="part-to-href"><xsl:with-param name="name">dynamic</xsl:with-param></xsl:call-template>#<xsl:value-of select="$name" />
				</xsl:otherwise>
			</xsl:choose>
		</xsl:attribute>
		<xsl:call-template name="name-to-dynvar">
			<xsl:with-param name="name"><xsl:value-of select="$name" /></xsl:with-param>
		</xsl:call-template>
	</xsl:element>
</xsl:template>
<xsl:template match="dynvar[@name and not(@extension)]">
	<xsl:call-template name="name-to-linked-dynvar">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="dynvar[@name and @extension]">
	<xsl:call-template name="name-to-linked-dynvar">
		<xsl:with-param name="extension"><xsl:value-of select="@extension" /></xsl:with-param>
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template name="name-to-state">
	<xsl:param name="name" />
	<varname>•<xsl:value-of select="$name" /></varname>
</xsl:template>
<xsl:template name="name-to-linked-state">
	<xsl:param name="name" />
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href"><xsl:call-template name="part-to-href"><xsl:with-param name="name">states</xsl:with-param></xsl:call-template>#<xsl:value-of select="$name" /></xsl:attribute>
		<xsl:call-template name="name-to-state">
			<xsl:with-param name="name"><xsl:value-of select="$name" /></xsl:with-param>
		</xsl:call-template>
	</xsl:element>
</xsl:template>
<xsl:template match="state[@name]">
	<xsl:call-template name="name-to-linked-state">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template name="name-to-type">
	<xsl:param name="name" />
	<typename>§<xsl:value-of select="$name" /></typename>
</xsl:template>
<xsl:template name="name-to-linked-type">
	<xsl:param name="name" />
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href"><xsl:call-template name="part-to-href"><xsl:with-param name="name">types</xsl:with-param></xsl:call-template>#<xsl:value-of select="$name" /></xsl:attribute>
		<xsl:call-template name="name-to-type">
			<xsl:with-param name="name"><xsl:value-of select="$name" /></xsl:with-param>
		</xsl:call-template>
	</xsl:element>
</xsl:template>
<xsl:template match="named-type[@name and not(@link)]">
	<xsl:call-template name="name-to-linked-type">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="named-type[@name and @link='no']">
	<xsl:call-template name="name-to-type">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template name="name-to-template-type">
	<xsl:param name="name" />
	<typename class="template">§<xsl:value-of select="$name" /></typename>
</xsl:template>
<xsl:template match="template-type[@name]">
	<xsl:call-template name="name-to-template-type">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template name="name-to-state-type">
	<xsl:param name="name" />
	<typename>§•<xsl:value-of select="$name" /></typename>
</xsl:template>
<xsl:template name="name-to-linked-state-type">
	<xsl:param name="name" />
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href"><xsl:call-template name="part-to-href"><xsl:with-param name="name">state-types</xsl:with-param></xsl:call-template>#<xsl:value-of select="$name" /></xsl:attribute>
		<xsl:call-template name="name-to-state-type">
			<xsl:with-param name="name"><xsl:value-of select="$name" /></xsl:with-param>
		</xsl:call-template>
	</xsl:element>
</xsl:template>
<xsl:template match="named-state-type[@name and not(@link)]">
	<xsl:call-template name="name-to-linked-state-type">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="named-state-type[@name and @link='no']">
	<xsl:call-template name="name-to-state-type">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template match="tol-param[@name and @link='no']">
	<tolparam><xsl:value-of select="@name" /></tolparam>
</xsl:template>
<xsl:template match="tol-param[@name and not(@link)]">
	<xsl:element name="a">
		<xsl:attribute name="class">discrete</xsl:attribute>
		<xsl:attribute name="href"><xsl:call-template name="part-to-href"><xsl:with-param name="name">algo-tol</xsl:with-param></xsl:call-template>#tol-<xsl:value-of select="@name" /></xsl:attribute>
		<tolparam><xsl:value-of select="@name" /></tolparam>
	</xsl:element>
</xsl:template>

<xsl:template name="name-to-operator">
	<xsl:param name="name" />
	<inline><xsl:value-of select="$name" /></inline>
</xsl:template>
<xsl:template match="operator[@name]">
	<xsl:call-template name="name-to-operator">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template name="name-to-operator-unary-prefix">
	<xsl:param name="name" />
	<inline><xsl:value-of select="$name" /></inline>
</xsl:template>
<xsl:template match="operator-unary-prefix[@name]">
	<xsl:call-template name="name-to-operator-unary-prefix">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
	</xsl:call-template>
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
<xsl:template match="replacable-text[@name]">
  <varname class="replacable"><xsl:value-of select="@name" /></varname>
</xsl:template>

<xsl:template name="name-to-argument">
	<xsl:param name="name" />
	<xsl:param name="class" />
	<xsl:element name="varname">
		<xsl:attribute name="class"><xsl:value-of select="$class" /></xsl:attribute>
		<xsl:value-of select="$name" />
	</xsl:element>
</xsl:template>
<xsl:template match="arg[@name]">
	<xsl:call-template name="name-to-argument">
		<xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param>
		<xsl:with-param name="class"><xsl:value-of select="@class" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>

<xsl:template match="state-arg[@name and not(@class)]">
  <varname>•<xsl:value-of select="@name" /></varname>
</xsl:template>
<xsl:template match="state-arg[@name and @class='replacable']">
  <varname class="replacable">•<xsl:value-of select="@name" /></varname>
</xsl:template>
<xsl:template match="field[@name and not(@class)]">
  <varname><xsl:value-of select="@name" /></varname>
</xsl:template>
<xsl:template match="field[@name and @class='replacable']">
  <varname class="replacable"><xsl:value-of select="@name" /></varname>
</xsl:template>
<xsl:template match="lexerregexp">
  <lexerregexp><xsl:apply-templates/></lexerregexp>
</xsl:template>
<xsl:template match="syntax[@name and @class='new']">
  <xsl:element name="a">
    <xsl:attribute name="name">stx/<xsl:value-of select="@name" /></xsl:attribute>
    <syntaxname class="new"><xsl:value-of select="@name"/></syntaxname>
  </xsl:element>
</xsl:template>
<xsl:template match="syntax[@name and not(@class)]">
  <xsl:element name="a">
    <xsl:attribute name="class">discrete</xsl:attribute>
    <xsl:attribute name="href"><xsl:call-template name="part-to-href"><xsl:with-param name="name">syntax</xsl:with-param></xsl:call-template>#stx/<xsl:value-of select="@name" /></xsl:attribute>
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
  <table class="tight"><xsl:apply-templates select="tr"/></table>
</xsl:template>
<xsl:template match="loose-table">
  <table class="loose"><xsl:apply-templates select="tr"/></table>
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
