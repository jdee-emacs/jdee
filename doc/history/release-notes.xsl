<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html" indent="yes" encoding="US-ASCII"
		doctype-public="-//W3C//DTD HTML 4.01//EN"
		doctype-system="http://www.w3.org/TR/html401/strict.dtd"/>

    <xsl:param name="version"/>
    <xsl:param name="title"/>

    <xsl:template match="/revision-log/release[@version=$version]">
	<html>
	    <head>
		<title><xsl:value-of select="$title"/></title>
		<style type="text/css">
		    body, p {
		    font-family: Verdana, Arial, Helvetica, sans-serif;
		    font-size: 80%;
		    color: #000000;
		    background-color: #ffffff;
		    }

		    li {
		    margin-top: 15px;
		    }
		</style>
	    </head>
	    <body>
		<h1>
		    <a name="top"><xsl:value-of select="$title"/></a>
		</h1>
		<xsl:if test="summary">
		    <xsl:value-of select="summary"/>
		    <hr/>
		</xsl:if>
		<ul>
		    <xsl:apply-templates select=".//change"/>
		</ul>
	    </body>
	</html>
    </xsl:template>

    <xsl:template match="map">
	<xsl:variable name="cities">
	    <xsl:for-each select="location">
		<xsl:value-of select="@city"/>
	    </xsl:for-each>
	</xsl:variable>
	<xsl:text>concatenated cities: </xsl:text>
	<xsl:value-of select="$cities"/>
    </xsl:template>

    <xsl:template match="change">
	<li>
	    <xsl:apply-templates select="msg"/>
	    <br/>
	    <xsl:choose>
		<xsl:when test="count(author) > 0">
		    <xsl:variable name="authors">
			<xsl:for-each select="author">
			    <xsl:value-of select="text()"/>,<xsl:text> </xsl:text>
			</xsl:for-each>
		    </xsl:variable>
		    Thanks to <xsl:value-of select="substring($authors, 0, string-length($authors) - 1)"/>
		</xsl:when>
		<xsl:otherwise>
		    Thanks to <xsl:value-of select="@commiter"/>
		</xsl:otherwise>
	    </xsl:choose>
	</li>
    </xsl:template>
</xsl:stylesheet>
