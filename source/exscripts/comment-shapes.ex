:1,1s!\(.*\)!/** \1!
:2,$s!\(.*\)! ** \1!
:$,$s!\(.*\)!\1\r **/\r!
:%s! $!!
"
:wq
