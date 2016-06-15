<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
		xmlns:ax="abc" 
		xmlns:xi="http://www.w3.org/2001/XInclude"
		xmlns:xhtml="http://www.w3.org/1999/xhtml"
version="1.0">

<!--
  <xsl:import href="ajw_utils.xsl" />
-->
<xsl:template match="/" >
  <dummy>
    <xsl:apply-templates />
  </dummy>
</xsl:template>

<xsl:template match='/site'>
   <xsl:comment>Generated web pages</xsl:comment>
     <xsl:comment>apply template to page set</xsl:comment>
   <xsl:apply-templates/>
</xsl:template>

<xsl:template match="//pages">
   <xsl:comment>Page set</xsl:comment>
     <xsl:element name="ax:file">
       <xsl:attribute name="filename">
   	<xsl:value-of select="concat(@id,'_nav.xml')" />
       </xsl:attribute>
	<xhtml:nav>
	  <xhtml:h2>Contact</xhtml:h2>
	  <xhtml:ul>
	    <xhtml:li><xsl:value-of select=".//contact-phone"/></xhtml:li>
	    <xhtml:li><xsl:copy-of  select=".//contact-email/xhtml:a"/></xhtml:li>
	    <xhtml:li>
	      <xhtml:a>
		<xsl:attribute name="href"><xsl:value-of select=".//homepage"/></xsl:attribute>Home Page</xhtml:a>	    
	    </xhtml:li>
	    <xsl:choose>
	      <xsl:when test="./navtype">
		<xsl:comment>calling meshindex</xsl:comment>
		<xsl:call-template name="meshindex" />
	      </xsl:when>
	      <xsl:when test="child::*[navtype='linear']">
		<xsl:call-template name="linearindex" />
	      </xsl:when>
	      <xsl:otherwise>
		Failed to match index template:
		<xsl:value-of select="./navtype"/> 
		Current node is:'<xsl:value-of select="string(self::node())" />'
	      </xsl:otherwise>
	    </xsl:choose>
	  </xhtml:ul>
	</xhtml:nav> 
     </xsl:element>
</xsl:template>


<xsl:template name="meshindex">
    <xsl:comment>in template meshindex</xsl:comment>
    <xsl:for-each select=".//xi:include">
      <xsl:variable name="pagefile_name" select="@href" />
      <xsl:variable name="pagedata" select="document($pagefile_name)" />
      <xhtml:li>
	<xhtml:a>
	<xsl:attribute name="href">
	  <xsl:value-of select="$pagedata//url"  />
	</xsl:attribute>
	<xsl:value-of select="$pagedata//name" />
	</xhtml:a>
      </xhtml:li>
    </xsl:for-each>
</xsl:template>

<xsl:template name="linearindex">
    Mesh linear index
    <a href="{(ancestor::pages)[1]/page[1]/url}">Home</a>
    <a href="{ancestor::pages/page[1]}">Start</a>
    <a href="{ancestor::page/preceding-sibling::page[1]/url}">Previous</a>
    <a href="{ancestor::page/following-sibling::page[1]/url}">Next</a>
</xsl:template>

</xsl:stylesheet>
