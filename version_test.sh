#!/bin/sh

VF=VERSION
CF=configure

VFN=$(sed '2,$ d' < $VF)
CFN=$(sed -e '/^PACKAGE_VERSION/ ! d' \
          -e 's/^PACKAGE_VERSION='"'"'\(.*\)'"'"'$/\1/' < $CF)

test "X$VFN" == "X$CFN" ; ret=$?

if test $ret -gt 0
then
		cat <<EOF
**********************************************************************
***  Version numbers in VERSION and configure.ac are not matching  ***
**********************************************************************
EOF
fi

exit $ret