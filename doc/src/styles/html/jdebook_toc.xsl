<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
	        xmlns:saxon="http://icl.com/saxon"
                exclude-result-prefixes="doc"
                version='1.0'>

<xsl:output method="text"/>
 

<xsl:template match="/">
<xsl:text>0  ; JDE Website                 ; 14 ; 3 ; http://jdee.sourceforge.net/   ; _top
</xsl:text>
<xsl:for-each select="book/chapter">
<xsl:text>0 ; </xsl:text><xsl:value-of select="normalize-space(title)"/><xsl:text> ; 0 ; ../html/jde-ug/jde-ug-content.html#</xsl:text><xsl:value-of select="title/anchor/@id"/><xsl:text>
</xsl:text>
<xsl:for-each select="sect3">
<xsl:text>1 ; </xsl:text><xsl:value-of select="normalize-space(title)"/><xsl:text> ; </xsl:text><xsl:choose><xsl:when test="count(sect4)=0">3</xsl:when><xsl:otherwise>0</xsl:otherwise></xsl:choose><xsl:text> ; ../html/jde-ug/jde-ug-content.html#</xsl:text><xsl:value-of select="title/anchor/@id"/><xsl:text>
</xsl:text>
<xsl:for-each select="sect4">
<xsl:text>2 ; </xsl:text><xsl:value-of select="normalize-space(title)"/><xsl:text> ; 3 ; ../html/jde-ug/jde-ug-content.html#</xsl:text><xsl:value-of select="title/anchor/@id"/><xsl:text>
</xsl:text>
</xsl:for-each>
</xsl:for-each>
</xsl:for-each>
</xsl:template>

<xsl:template match="text()|@*">
</xsl:template>

</xsl:stylesheet>

