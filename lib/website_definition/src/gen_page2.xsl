<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="xml" version="1.0" encoding="ISO-8859-1" indent="yes" />
  <xsl:variable name="sitedata_file" select="'../site_data/pagedata.xml'" />
  <xsl:variable name="sitedata" select="document($sitedata_file)/site/[not pages]" />
  <xsl:variable name="url" select="//url" />
  <xsl:variable name="pages" 
	      select="document($sitedata_file)//pages[/page//include/@href='index.xml']"/>

  <xsl:template match="page">
    <xsl:value-of select="$pages/file_headers" />
    <xhtml:html  xmlns:xhtml="http://www.w3.org/1999/xhtml">
      <xhtml:head>
	<xhtml:meta name="Keywords" content='{keywords}' />
	<xhtml:title>
	  <xsl:value-of select="./title" />
	</xhtml:title>
	<xhtml:meta name="viewport" content="width=device-width, initial-scale=1" />
	<xhtml:style type="text/css">
	  body {color:blue;}  
	  p  {line-height=130%;}
	  h1 {text-align:center;}
	  h2 {text-align:center;}
	  h3 {text-align:center;}
	  pre { color:#800080;}
	  a:link {color:#0000FF;}
	  a:visited {color:#C0C000;}
	  nav {
	    background-image:url(images/grgcleft2.png);
	    position: absolute;
	    top :0px;
	    bottom:0;
	    left: 0;
	    width: 240px;
	    }
	 section {
	    position: relative;
	    margin-left: 250px;
	    }
	</xhtml:style>
	<xsl:copy-of select="./model" />

      </xhtml:head>
      <xhtml:body>
	<xhtml:nav>
	  <xhtml:h2>Contact</xhtml:h2>
	  <xhtml:ul>
	    <xhtml:li><xsl:value-of select="ancestor::pages/contact-phone"/></xhtml:li>
	    <xhtml:li><xsl:copy-of  select="ancestor::pages/contact-email/*"/></xhtml:li>
	    <xhtml:li>
	      <xhtml:a><xsl:attribute name="href"><xsl:value-of select="ancestor::pages/homepage"/></xsl:attribute>Home Page</xhtml:a>
	    </xhtml:li>
	    <xsl:choose>
	      <xsl:when test="ancestor::pages[navtype='mesh']">
		<xsl:call-template name="meshindex" />
	      </xsl:when>
	      <xsl:when test="ancestor::pages[navtype='linear']">
		<xsl:call-template name="linearindex" />
	      </xsl:when>
	      <xsl:otherwise>
		Failed to match index template:
		<xsl:value-of select="ancestor::pages/navtype"/> 
		Current node is:
		<xsl:value-of select="string(./node())" />
	      </xsl:otherwise>
	    </xsl:choose>
	  </xhtml:ul>
	</xhtml:nav> 
	<xhtml:section>
	  <xsl:message>section</xsl:message>
	  <xhtml:a>
	    <xsl:attribute name="href">
	      <xsl:value-of select="ancestor::pages/homepage"/>
	    </xsl:attribute>
	    <xsl:copy-of select="ancestor::pages/banner_image/child::xhtml:img"/>
	  </xhtml:a>
	  <xhtml:br />
	  <xsl:copy-of select="./ax:content/*"/>
	</xhtml:section>
      </xhtml:body>
      </xhtml:html>

  </xsl:template>
</xsl:transform>
  
