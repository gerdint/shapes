noinst_PROGRAMS = postproc-html postproc-man

AM_LDFLAGS = -ll
AM_LFLAGS = -olex.yy.c
AM_YFLAGS = -dvt

postproc_html_SOURCES = html.l
postproc_man_SOURCES = man.l

dist-hook : postproc-html${EXEEXT} postproc-man${EXEEXT} 
