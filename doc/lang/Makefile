all: coretypes.html

coretypes.html: coretypes.xml types_html.xsl types.dtd
	xmlto -x types_html.xsl html coretypes.xml
	mv coretypes.proc $@
