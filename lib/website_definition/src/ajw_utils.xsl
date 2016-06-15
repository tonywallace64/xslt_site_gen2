<!--
Author:  Tony Wallace
Contact: tony@tony.gen.nz

Licence: This program is donated to the public domain.
Freely I have received, freely I give.

I acknowledge the work of the w3 consortium that made
this work possible.
-->
<!--
A few xslt utilities..

The cdata tag encloses the result of apply templates
into a ddata tag with a the data quoted by CDATA tags.
A ddata maintains the CDATA tags through processing
steps and stops the CDATA tags from disappearing by
putting ddata into the output cdata-section-elements.
-->
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns:ax="abc">
  
<xsl:output method = "xml"/>
<xsl:template match="/">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="@*">
  <xsl:attribute name="{name()}">
    <xsl:message>create attribute <xsl:value-of select="name()"/> = 
         <xsl:value-of select="." /> </xsl:message>
    <xsl:value-of select="."/>
  </xsl:attribute>
</xsl:template>

<xsl:template match="text()">
  <xsl:value-of select="."/>
</xsl:template>

<xsl:template match="node()">
  <xsl:message>general match <xsl:value-of select="name()"/></xsl:message>
  <xsl:element name="{name()}">
    <xsl:apply-templates/>
  </xsl:element>
</xsl:template>

<xsl:template match="text()" name="insertBreaks" mode="crtobr">
  <xsl:param name="pText" select="."/>

  <xsl:choose>
    <xsl:when test="not(contains($pText, '&#xA;'))">
      <xsl:copy-of select="$pText"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="substring-before($pText, '&#xA;')"/>
      <br />
      <xsl:call-template name="insertBreaks">
	<xsl:with-param name="pText" select=
	  "substring-after($pText, '&#xA;')"/>
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
</xsl:stylesheet>
