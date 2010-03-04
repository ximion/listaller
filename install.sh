#!/bin/bash
# ==============================================================================
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; version 3 of the License.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# ==============================================================================
# (C) 2009 Christian Beuschel <chris109(at)web.de>

# Configuring destionation directory
INSTDIR="/opt/pappi"

# Configuration for mime-type registration
MIMEBASE="/usr/local/share/mime"
MIMEDB=$MIMEBASE"/packages"
MIMEFILE="pappi.xml"
APPDB="/usr/local/share/applications"
APPLIST="defaults.list"
APPLINIK_I="pappi_install.desktop"
APPLINIK_T="pappi_trust.desktop"

# Reading information about the distribution
LSB_RELEASE_FILE="/etc/lsb-release"

if [ -f $LSB_RELEASE_FILE ]; then
	source $LSB_RELEASE_FILE
else
	echo "Error: Installation LSB release file \"$LSB_RELEASE_FILE\" has not been found!"
	exit 1
fi

# Installation

INSTALLER_DIR="./distributions"
SCRIPT=$INSTALLER_DIR'/'$DISTRIB_ID'_'$DISTRIB_RELEASE


if [ -f $SCRIPT ]; then
	source $SCRIPT
else
	echo "Error: Your Distribution or your Version of $DISTRIB_ID is not supported yet. If you are a programmer, help us to support it."
	exit 1
fi

exit 0
