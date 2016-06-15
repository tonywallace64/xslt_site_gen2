<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:transform version="1.0" 
	       xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	       xmlns:xi="http://www.w3.org/2001/XInclude"
	       xmlns:exslt="http://exslt.org/strings"
	       >
  <xsl:output method="xml" version="1.0" encoding="ISO-8859-1" indent="yes" />
  <xsl:variable name="sitedata_file" select="'../site_data/pagedata.xml'" />
  <xsl:variable name="site" select="document($sitedata_file)/site/*[not (pages)]" />
  <xsl:variable name="url" select="//url" />
  <xsl:variable name="source" select="//source/text()" />
  <xsl:variable name="pages" 
	      select="document($sitedata_file)//pages[xi:include/@href=$source]" />
  <xsl:template match="/">
    <xsl:message>sitedata_file <xsl:value-of select="$sitedata_file" /> </xsl:message>
    <xsl:message>site <xsl:value-of select="$site" /> </xsl:message>
    <xsl:message>url <xsl:value-of select="$url" /> </xsl:message>
    <xsl:message>source <xsl:value-of select="$source" /> </xsl:message>
    <xsl:message>pages <xsl:value-of select="$pages" /> </xsl:message>
    <xsl:value-of select="$site/html_header" />
    <xsl:text disable-output-escaping="yes"><![CDATA[<!DOCTYPE html>]]></xsl:text>
    <xhtml:html  xmlns:xhtml="http://www.w3.org/1999/xhtml">
      <xhtml:head>
	<xhtml:meta name="Keywords" content='{keywords}' />
	<xhtml:title>
	  <xsl:value-of select="//title" />
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
	<xhtml:div id="navbar">
	</xhtml:div> 
	<xhtml:section>
	  <xhtml:a>
	    <xsl:attribute name="href">
	      <xsl:value-of select="$pages/homepage"/>
	    </xsl:attribute>
	    <xsl:copy-of select="$pages/banner_image/child::xhtml:img"/>
	  </xhtml:a>
	  <xhtml:br />
	  <xsl:copy-of select="//content/*" />
	</xhtml:section>
<xhtml:script>
//<![CDATA[  test1
  function writeNavbar(Contents)
  {
  document.getElementById("navbar").innerHTML=Contents;
  };
  function httpGetAsync(theUrl, callback)
  {
    var xmlHttp = new XMLHttpRequest();
    xmlHttp.onreadystatechange = function() { 
        if (xmlHttp.readyState == 4 && xmlHttp.status == 200)
            callback(xmlHttp.responseText);
    }
    xmlHttp.open("GET", theUrl, true); // true for asynchronous 
    xmlHttp.send(null);
  };
  httpGetAsync(
//]]>
  <xsl:text disable-output-escaping="yes">"</xsl:text>
  <xsl:value-of select="$pages/@id" />
  <xsl:text disable-output-escaping="yes">_nav.xml",writeNavbar);</xsl:text>
</xhtml:script>
      </xhtml:body>
      </xhtml:html>

  </xsl:template>
</xsl:transform>
  
