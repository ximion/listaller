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
LSB_RELEASE_BIN_FILE="/usr/bin/lsb_release"
FEDORA_RELEASE_FILE="/etc/fedora-release"

if [ -f $LSB_RELEASE_FILE ]; then
   source $LSB_RELEASE_FILE
elif [ -e $FEDORA_RELEASE_FILE ]; then 
   DISTRIB_ID="Fedora" 
   DISTRIB_RELEASE=`rpm -q --qf "%{version}\n" fedora-release`
elif [ -f $LSB_RELEASE_BIN_FILE ]; then
   DISTRIB_ID=$(lsb_release -s -i)
   # -c gives the codename of the distro, -r would give release number.
   # Better to use the codename: not interested in having an install script for
   # every release candidate and every security update of a stable release...
   DISTRIB_RELEASE=$(lsb_release -s -c)
else
   echo -n "Error: Installation LSB release file \"$LSB_RELEASE_FILE\" "
   echo "or fedora release file \"$FEDORA_RELEASE_FILE\" has not been found!"
   exit 1
fi

# Installation

INSTALLER_DIR="./distributions"
SCRIPT=$INSTALLER_DIR'/'$DISTRIB_ID'_'$DISTRIB_RELEASE

# Are we user 'root'?
if [ "$UID" != "0" ]; then
   echo "Error: You'd better be root in order to install this application!"
   exit -1
fi

if [ -f $SCRIPT ]; then
   source $SCRIPT
else
   echo -n "Error: Your Distribution or your Version of "
   echo -n "$DISTRIB_ID/$DISTRIB_RELEASE is not "
   echo "supported yet. If you are a programmer, help us to support it."
   exit 1
fi

exit 0
