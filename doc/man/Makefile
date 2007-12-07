all: mini.proc

mini.proc: man.dtd mini.sxml man.xsl
	xmlto --skip-validation -x man.xsl html mini.sxml
