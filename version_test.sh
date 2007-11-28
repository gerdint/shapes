#!/bin/sh

VF=VERSION
CF=configure

VFN=$(sed '2,$ d' < $VF)
CFN=$(sed -e '/^PACKAGE_VERSION=/ ! d' \
          -e 's/^PACKAGE_VERSION='"'"'\(.*\)'"'"'$/\1/' < $CF)

test "X$VFN" == "X$CFN" ; ret=$?

if test $ret -gt 0
then
		cat <<EOF
**********************************************************************
***  Version numbers in VERSION and configure.ac are not matching: ***
***    $VFN vs $CFN                                                ***
***  Force a                                                       ***
***    make version_sync                                           ***
***  or, if that doesn't work,                                     ***
***    make -f Makefile.am version_sync                            ***
***  before any other make commands.                               ***
**********************************************************************
EOF
fi

exit $ret
