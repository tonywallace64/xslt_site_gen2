<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:transform version="1.0" 
	       xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	       xmlns:xi="http://www.w3.org/2001/XInclude"
	       >
<xsl:output method="xml" indent="yes" omit-xml-declaration="yes" />
<xsl:template match="/" >
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="text()" />

<xsl:template match="//xi:include" >
  <xsl:value-of select="@href"/>
  <xsl:text>&#x20;</xsl:text>
</xsl:template>
</xsl:transform>
