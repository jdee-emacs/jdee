<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<xsl:import href="file:///c:/home/xae-dev/xae/doctypes/docbook/styles/docbook/html/docbook.xsl" />

<xsl:variable name="chapter.autolabel">0</xsl:variable>
<xsl:variable name="generate.division.toc">0</xsl:variable>
<xsl:variable name="generate.component.toc">0</xsl:variable>
<xsl:param name="formal.procedures" select="0"/>
<xsl:param name="make.year.ranges" select="1"/>




<xsl:template match="*" mode="process.root">
  <xsl:variable name="doc" select="self::*"/>
  <html>
  <link rel="StyleSheet" href="../css/jde_style.css" TYPE="text/css"></link>
  <head>
    <xsl:call-template name="head.content">
      <xsl:with-param name="node" select="$doc"/>
    </xsl:call-template>
    <xsl:call-template name="user.head.content">
      <xsl:with-param name="node" select="$doc"/>
    </xsl:call-template>
  </head>
  <body>
    <xsl:call-template name="body.attributes"/>
    <xsl:call-template name="user.header.content">
      <xsl:with-param name="node" select="$doc"/>
    </xsl:call-template>
    <xsl:apply-templates select="."/>
    <xsl:call-template name="user.footer.content">
      <xsl:with-param name="node" select="$doc"/>
    </xsl:call-template>
  </body>
  </html>
</xsl:template>


<xsl:template match="ulink">
  <a>
    <xsl:if test="@id">
      <xsl:attribute name="name"><xsl:value-of select="@id"/></xsl:attribute>
    </xsl:if>
    <xsl:attribute name="href"><xsl:value-of select="@url"/></xsl:attribute>
    <xsl:if test="@type">
      <xsl:attribute name="target"><xsl:value-of select="@type"/></xsl:attribute>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="string-length(.)=0">
	<xsl:value-of select="@url"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </a>
</xsl:template>

<xsl:template match="guimenu">
  <xsl:call-template name="inline.boldseq"/>
</xsl:template>


<xsl:template match="guimenuitem">
  <xsl:call-template name="inline.boldseq"/>
</xsl:template>

<xsl:template match="guisubmenu">
  <xsl:call-template name="inline.boldseq"/>
</xsl:template>


<xsl:template match="command">
  <xsl:call-template name="inline.monoseq"/>
</xsl:template>



</xsl:stylesheet>

