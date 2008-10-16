ACLOCAL_AMFLAGS = -I m4

SUBDIRS			= source doc resources test
CLEANFILES		= configure-stamp build-stamp
BUILT_SOURCES = version_test

dist_noinst_DATA        = m4/lib-ld.m4 m4/lib-prefix.m4 m4/lib-link.m4 \
                          m4/iconv.m4 m4/zlib.m4 \
                          m4/bison.m4 m4/flex.m4 \
                          m4/gsl.m4 m4/opt.m4 \
                          config/config.rpath \
                          VERSION
dist_noinst_SCRIPTS     = config/oldtool

.PHONY: version_sync
version_sync:
	-sed -e 's/VERSION/$(shell sed "2,$$ d" < VERSION)/' < source/exscripts/exconfig | ex -sC configure.ac

version_test : FORCE
	@if test -x version_test.sh ;\
	then \
		./version_test.sh ;\
	fi

FORCE :
