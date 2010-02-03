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

# Installation

echo " "

echo -n "Copying files ..."

mkdir $INSTDIR
cp -r ./files/* $INSTDIR/
chmod +x $INSTDIR/bin/pappi
chmod +x $INSTDIR/bin/uninstall
ln -s $INSTDIR/bin/pappi /usr/local/bin/pappi

echo '... done'
echo " "

echo -n "Registering mime-types ..."

if [ ! -d "$MIMEDB" ]; then
	mkdir -p $MIMEDB
fi
if [ ! -d "$APPDB" ]; then
	mkdir -p $APPDB
fi

echo '<?xml version="1.0" encoding="UTF-8"?>' >> $MIMEDB'/'$MIMEFILE

echo '<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">' >> $MIMEDB'/'$MIMEFILE
echo '  <mime-type type="application/x-personal-application-package">' >> $MIMEDB'/'$MIMEFILE
echo '    <comment>Personal Application Package</comment>' >> $MIMEDB'/'$MIMEFILE
echo '    <glob pattern="*.pappp"/>' >> $MIMEDB'/'$MIMEFILE
echo '    <generic-icon name="package-x-generic"/>' >> $MIMEDB'/'$MIMEFILE
echo '  </mime-type>' >> $MIMEDB'/'$MIMEFILE

echo '  <mime-type type="application/x-personal-application-store-trust-file">' >> $MIMEDB'/'$MIMEFILE
echo '    <comment>Personal Application Store Trust File</comment>' >> $MIMEDB'/'$MIMEFILE
echo '    <glob pattern="*.pappst"/>' >> $MIMEDB'/'$MIMEFILE
echo '    <generic-icon name="package-x-generic"/>' >> $MIMEDB'/'$MIMEFILE
echo '  </mime-type>' >> $MIMEDB'/'$MIMEFILE

echo '</mime-info>' >> $MIMEDB'/'$MIMEFILE

if [ ! -f $APPDB/$APPLIST ]; then
	echo "[Default Applications]" > $APPDB/$APPLIST
fi
echo "application/x-personal-application-package=pappi_install.desktop" >> $APPDB/$APPLIST
echo "application/x-personal-application-store-trust-file=pappi_trust.desktop" >> $APPDB/$APPLIST

echo '[Desktop Entry]' >> $APPDB/$APPLINIK_I
echo 'Version=1.0' >> $APPDB/$APPLINIK_I
echo 'Encoding=UTF-8' >> $APPDB/$APPLINIK_I
echo 'Name=Personal App Installer' >> $APPDB/$APPLINIK_I
echo 'Terminal=false' >> $APPDB/$APPLINIK_I
echo 'Exec=/opt/pappi/bin/pappi -i %U' >> $APPDB/$APPLINIK_I
echo 'Icon=/opt/pappi/icons/pappi_48.png' >> $APPDB/$APPLINIK_I
echo 'Type=Application' >> $APPDB/$APPLINIK_I


update-mime-database $MIMEBASE

echo '... done'
echo " "

echo "Installing dependenzies ..."

apt-get -y install uae wine dosbox zenity

echo '... done'

echo 'Installation of pappi complete.'

exit 0
