<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html" indent="yes" encoding="US-ASCII"
		doctype-public="-//W3C//DTD HTML 4.01//EN"
		doctype-system="http://www.w3.org/TR/html401/strict.dtd"/>

    <xsl:param name="version"/>
    <xsl:param name="title"/>

    <xsl:template match="/">
	<xsl:for-each select="/revision-log/release[@version=$version]">
	    <xsl:call-template name="release"/>
	</xsl:for-each>
    </xsl:template>

    <xsl:template name="release">
	<html>
	    <head>
		<title><xsl:value-of select="$title"/></title>
		<style type="text/css">
		    body {
		    font-family: Arial, sans-serif;
		    font-size: 11pt
		    }

		    h1 {
		    font-family: Arial, sans-serif;
		    background-color: #d4d4d4;
		    font-weight: bold;
		    font-size: 18pt
		    }

		    h2 {
		    font-style: italic;
		    font-family: Arial, sans-serif;
		    margin-bottom: 1px;
		    font-size: 18pt;
		    border-bottom-color: black;
		    border-bottom-style: solid;
		    border-bottom-width: thin
		    }

		    div.code {
		    font-family: "Courier New", serif;
		    font-size: 10pt;
		    background-color: #d4d4d4;
		    border-color: black;
		    border-width: 1px;
		    border-style: dotted;
		    #            margin-right: 100%;
		    margin-left: 20pt;
		    margin-top: 5pt;
		    margin-bottom: 5pt;
		    }

		    div.code pre {
		    margin-top: 5pt;
		    margin-bottom: 5pt;
		    margin-left: 10pt;
		    margin-right: 10pt;
		    }

		    li {
		    margin-bottom: 1%;
		    }

		    div.component li {
		    margin-bottom: 0pt;
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

    <xsl:template match="change">
	<li>
	    <xsl:copy-of select="msg/node()"/>
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
