<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="system-binding[@identifier]">
	<div>
		<p>
			<xsl:element name="table">
				<xsl:attribute name="class">
					<xsl:choose>
						<xsl:when test="function">function</xsl:when>
						<xsl:when test="simple-value">simple</xsl:when>
						<xsl:when test="hot">hotvalue</xsl:when>
						<xsl:otherwise>loose</xsl:otherwise>
					</xsl:choose>
				</xsl:attribute>
				<tr>
					<th class="big" colspan="2">
						<xsl:element name="a">
							<xsl:attribute name="name">bind/<xsl:value-of select="@identifier" /></xsl:attribute>
							<xsl:call-template name="name-to-binding"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
						</xsl:element>
					</th>
				</tr>
				<xsl:apply-templates />
			</xsl:element>
		</p>
	</div>
</xsl:template>

<xsl:template match="system-binding[@identifier]/function | mutator/function">
	<xsl:if test="top">
 		<td colspan="2"><xsl:apply-templates select="top"/></td>
	</xsl:if>
 	<xsl:apply-templates select="case"/>
	<xsl:if test="body">
 		<td colspan="2"><xsl:apply-templates select="body"/></td>
	</xsl:if>
</xsl:template>

<xsl:template match="system-binding[@identifier]/function/case | mutator/function/case">
	<tr>
		<th class="heading" colspan="2">
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
		</th>
	</tr>
 	<xsl:apply-templates select="type-templates/*"/>
 	<xsl:apply-templates select="dynamic-references"/>
	<xsl:apply-templates select="side-effect"/>
	<tr>
		<td colspan="2">
			<xsl:apply-templates select="description" />
		</td>
	</tr>
 	<xsl:apply-templates select="see-also"/>
</xsl:template>

<xsl:template match="dynamic-references[not(dynvar | dynstate)]">
	<tr>
		<th class="horiz">Dynamic references:</th>
		<td><em>none</em></td>
	</tr>
</xsl:template>
<xsl:template match="dynamic-references[dynvar | dynstate]">
	<tr>
		<th class="horiz">Dynamic references:</th>
		<td><xsl:apply-templates /></td>
	</tr>
</xsl:template>
<xsl:template match="dynamic-references/dynstate[@name='all']">
	<xsl:text>&lt;The entire dynamic state&gt;</xsl:text>
</xsl:template>
<xsl:template match="dynamic-references/dynstate[@name='graphics']">
	<xsl:text>&lt;The graphics dynamic state&gt;</xsl:text>
</xsl:template>
<xsl:template match="dynamic-references/dynstate[@name='text']">
	<xsl:text>&lt;The text dynamic state&gt;</xsl:text>
</xsl:template>

<xsl:template match="side-effect">
	<tr>
		<th class="horiz">Side effect:</th>
		<td><xsl:apply-templates /></td>
	</tr>
</xsl:template>

<xsl:template match="system-binding[@identifier]/hot">
	<tr>
		<th class="note" colspan="2">This is a hot value.</th>
	</tr>
	<tr>
		<th class="horiz">Spawns:</th>
		<td><xsl:apply-templates select="constructor-of" /></td>
	</tr>
</xsl:template>

<xsl:template match="system-binding[@identifier]/simple-value">
	<tr>
		<th class="horiz">Type:</th>
		<td><xsl:apply-templates select="type"/></td>
	</tr>
	<tr>
		<td colspan="2">
			<xsl:apply-templates select="description"/>
		</td>
	</tr>
	<xsl:if test="type/named-type">
		<xsl:variable name="self"><xsl:value-of select="../@identifier" /></xsl:variable>
		<xsl:variable name="t"><xsl:value-of select="type/named-type/@name" /></xsl:variable>
 		<tr>
			<th class="horiz">Related by type:</th>
			<td>
				<xsl:for-each select="//system-binding[@identifier!=$self]/simple-value/type/named-type[@name=$t]">
					<xsl:text>  </xsl:text><xsl:call-template name="name-to-linked-binding"><xsl:with-param name="name"><xsl:value-of select="../../../@identifier" /></xsl:with-param></xsl:call-template>
				</xsl:for-each>
			</td>
		</tr>
	</xsl:if>
	<xsl:apply-templates select="see-also"/>
</xsl:template>

<xsl:template match="system-binding//see-also | core-state-type/see-also | mutator//see-also">
 	<tr>
		<th class="horiz">See also:</th>
		<td>
			<xsl:for-each select="./*">
				<xsl:text>  </xsl:text><xsl:apply-templates select="."/>
			</xsl:for-each>
		</td>
	</tr>
</xsl:template>


<xsl:template match="body/dynamic-variable[@identifier] | section/dynamic-variable[@identifier]">
  <xsl:variable name="self">
    <xsl:value-of select="@identifier" />
  </xsl:variable>
	<div>
		<p>
			<table class="dynvars">
				<tr>
					<th class="big" colspan="2">
						<xsl:element name="a">
							<xsl:attribute name="name">dyn/<xsl:value-of select="@identifier" /></xsl:attribute>
							<xsl:call-template name="name-to-dynvar"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
						</xsl:element>
					</th>
				</tr>
				<tr>
					<th class="horiz">Used by:</th>
					<td>
						<xsl:choose>
							<xsl:when test="/book/external">
								<xsl:for-each select="/book/external/section/system-binding[@identifier]">
									<xsl:if test="function/case/dynamic-references/dynvar[@name=$self]">
										<xsl:call-template name="name-to-linked-binding"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
										<xsl:text> </xsl:text>
									</xsl:if>
								</xsl:for-each>
							</xsl:when>
							<xsl:otherwise>
								<i>Not implemented!</i>
							</xsl:otherwise>
						</xsl:choose>
					</td>
				</tr>
				<xsl:apply-templates select="type" />
				<xsl:apply-templates select="default" />
				<xsl:apply-templates select="constraint" />
				<tr>
					<td colspan="2">
						<xsl:apply-templates select="description" />
					</td>
				</tr>
			</table>
		</p>
	</div>
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/type">
	<tr>
		<th class="horiz">Type:</th>
		<td><xsl:apply-templates /></td>
	</tr>
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/default">
	<tr>
		<th class="horiz">Default binding:</th>
		<td><xsl:apply-templates /></td>
	</tr>
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/constraint">
	<tr>
		<th class="horiz">Constraint:</th>
		<td><xsl:apply-templates /></td>
	</tr>
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/description">
	<xsl:apply-templates />
</xsl:template>

<xsl:template match="dynamic-variable[@identifier]/constraint/self">
	<xsl:call-template name="name-to-dynvar"><xsl:with-param name="name"><xsl:value-of select="../../@identifier" /></xsl:with-param></xsl:call-template>
</xsl:template>

<xsl:template match="dynamic-variable-table">
  <table class="dynvars">
		<tr>
			<th>Binding</th>
			<th>Type</th>
			<th>Default</th>
			<th>Description</th>
		</tr>
		<xsl:apply-templates select="dynamic-variable"/>
	</table>
</xsl:template>
<xsl:template match="dynamic-variable-table/dynamic-variable">
	<tr>
		<td>
			<xsl:element name="a">
				<xsl:attribute name="name">dyn/<xsl:value-of select="@identifier" /></xsl:attribute>
				<xsl:call-template name="name-to-dynvar"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
			</xsl:element>
		</td>
		<td><xsl:apply-templates select="type/*"/></td>
		<td><xsl:apply-templates select="default/*"/></td>
		<td><xsl:apply-templates select="description"/></td>
	</tr>
</xsl:template>

<xsl:template match="system-state[@identifier]">
	<div>
		<p>
			<xsl:element name="table">
				<xsl:attribute name="class">state</xsl:attribute>
				<tr>
					<th class="big" colspan="2">
						<xsl:element name="a">
							<xsl:attribute name="name">state/<xsl:value-of select="@identifier" /></xsl:attribute>
							<xsl:call-template name="name-to-state"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
						</xsl:element>
					</th>
				</tr>
				<tr>
					<th class="horiz">Type:</th>
					<td><xsl:apply-templates select="type"/></td>
				</tr>
				<tr>
					<td colspan="2">
						<xsl:apply-templates select="description/*" />
					</td>
				</tr>
			</xsl:element>
		</p>
	</div>
</xsl:template>

<xsl:template match="system-state[@identifier]/description/p/self">
	<xsl:call-template name="name-to-state"><xsl:with-param name="name"><xsl:value-of select="../../../@identifier" /></xsl:with-param></xsl:call-template>
</xsl:template>


<xsl:template match="core-state-type[@name]">
	<div>
		<p>
			<xsl:element name="table">
				<xsl:attribute name="class">state-type</xsl:attribute>
				<tr>
					<th class="big" colspan="2">
						<xsl:element name="a">
							<xsl:attribute name="name">state-type/<xsl:value-of select="@name" /></xsl:attribute>
							<xsl:call-template name="name-to-state-type"><xsl:with-param name="name"><xsl:value-of select="@name" /></xsl:with-param></xsl:call-template>
						</xsl:element>
					</th>
				</tr>
				<xsl:apply-templates select="abstraction" />
				<xsl:if test="description">
					<tr>
						<td colspan="2">
							<xsl:apply-templates select="description/*" />
						</td>
					</tr>
				</xsl:if>
				<xsl:apply-templates select="see-also"/>
				<tr>
					<th class="big" colspan="2">Mutators</th>
				</tr>
				<tr>
					<td colspan="2">
						<table class="mutator">
							<tr>
								<th class="big" colspan="2"><inline>• &lt;&lt;</inline></th>
							</tr>
							<xsl:apply-templates select="mutator[@op='tack-on']" />
							<xsl:if test="not(mutator[@op='tack-on'])">
								<tr>
									<td colspan="2">
										<p>There are no tack-on mutators.</p>
									</td>
								</tr>
							</xsl:if>
						</table>
					</td>
				</tr>
				<tr>
					<td colspan="2">
						<table class="mutator">
							<tr>
								<th class="big" colspan="2"><inline>(•)</inline></th>
							</tr>
							<xsl:apply-templates select="mutator[@op='peek']" />
							<xsl:if test="not(mutator[@op='peek'])">
								<tr>
									<td colspan="2">
										<p>The state cannot be peeked.</p>
									</td>
								</tr>
							</xsl:if>
						</table>
					</td>
				</tr>
				<tr>
					<td colspan="2">
						<table class="mutator">
							<tr>
								<th class="big" colspan="2"><inline>•;</inline></th>
							</tr>
							<xsl:apply-templates select="mutator[@op='freeze']" />
							<xsl:if test="not(mutator[@op='freeze'])">
								<tr>
									<td colspan="2">
										<p>The state cannot be frozen.</p>
									</td>
								</tr>
							</xsl:if>
						</table>
					</td>
				</tr>
				<xsl:for-each select="mutator[@identifier]">
					<tr>
						<td colspan="2">
							<table class="mutator">
								<tr>
									<th class="big" colspan="2">
										<xsl:element name="a">
											<xsl:attribute name="name">mutator/<xsl:value-of select="./../@name" />/<xsl:value-of select="@identifier" /></xsl:attribute>
											<xsl:call-template name="name-to-mutator"><xsl:with-param name="name"><xsl:value-of select="@identifier" /></xsl:with-param></xsl:call-template>
										</xsl:element>
									</th>
								</tr>
								<xsl:apply-templates />
							</table>
						</td>
					</tr>
				</xsl:for-each>
			</xsl:element>
		</p>
	</div>
</xsl:template>

<xsl:template match="core-state-type[@name]/abstraction">
	<tr>
		<th class="horiz">Abstraction:</th>
		<td><xsl:apply-templates /></td>
	</tr>
</xsl:template>

<xsl:template match="core-state-type[@name]/abstraction/p/self">
	<xsl:call-template name="name-to-state-type">
		<xsl:with-param name="name"><xsl:value-of select="../../../@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>
<xsl:template match="core-state-type[@name]/description/p/self">
	<xsl:call-template name="name-to-state-type">
		<xsl:with-param name="name"><xsl:value-of select="../../../@name" /></xsl:with-param>
	</xsl:call-template>
</xsl:template>


</xsl:stylesheet>
