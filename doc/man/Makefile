all: mini.proc

mini.proc: man.dtd mini.sxml
	xmlto --skip-validation -x man.xsl html mini.sxml
