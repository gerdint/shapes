:%s?\(.*\)?<!-- \1                                                                                           ?
:%s?\(<!-- .\{70\}\).*?\1 -->?
:$,$s!\(.*\)!\1\r!
"
:wq
