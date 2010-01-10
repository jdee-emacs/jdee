<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="xml" encoding="utf-8" indent="yes"/>

    <xsl:param name="version"/>

    <xsl:template match="/revision-log/release[@version=$version]">
	<release>
	    <revision>
		<xsl:call-template name="repo"/>
	    </revision>
	</release>
    </xsl:template>

    <xsl:template name="repo">
	<start><xsl:value-of select="repo/@start-revision"/></start>
	<end><xsl:value-of select="repo/@end-revision"/></end>
    </xsl:template>
</xsl:stylesheet>
