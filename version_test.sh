#!/bin/sh

# This file is part of Shapes.
#
# Shapes is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# Shapes is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Shapes.  If not, see <http://www.gnu.org/licenses/>.
#
# Copyright 2008 Gustaf Hendeby

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
