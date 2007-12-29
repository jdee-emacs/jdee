<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<xsl:template match="/">
<html>
<head>
<link rel="StyleSheet" href="../css/jde_style.css" TYPE="text/css"></link>
</head>
<body bgcolor="#FFF7C8">
<h2>Contents</h2>
<applet codebase="../../tli_rbl" archive="tli_rbl.jar" code="tli_rbl.class" width="300" height="800">

    <param name="file"         value="./txt/jde-ug-toc.txt"></param>
    <param name="target"       value="content"></param>
    <param name="csep"         value=";"></param>
    <param name="bImg"         value="../tli_rbl/img/icon3.gif"></param>
    <param name="bgcolor"      value="16775112"></param>

    <param name="v0" 	     value="http://www.javaside.com"></param>
    <param name="v1"	     value="_top"></param>
    <param name="font0"	     value="Arial, 0, 12, 0, -255"></param>
    <param name="font1"	     value="Times, 0, 12, 0, 12000"></param>
    <param name="font2"	     value="Arial, 2, 14, 255, -255"></param>
    <param name="font3"	     value="Arial, 3, 14, 55, 1255"></param>

<table border="0" width="100%" cols="1">

<tbody>
<xsl:apply-templates/>
</tbody>
</table>

</applet>
</body>
</html>
</xsl:template>


<xsl:template match="chapter">
<tr>
<td>
<a>
<xsl:attribute name="href">
<xsl:text>jde-ug-content.html#</xsl:text><xsl:value-of select="title/anchor/@id"/>
</xsl:attribute>
<xsl:attribute name="target">
<xsl:text>content</xsl:text>
</xsl:attribute>
<xsl:value-of select="title"/>
</a>
</td>
</tr>
</xsl:template>


<xsl:template match="text()|@*">
</xsl:template>



</xsl:stylesheet>

