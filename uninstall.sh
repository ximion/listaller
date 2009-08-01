#!/bin/bash
# Removes all files which are installed by listaller-install
set -e

echo "Uninstallation started."
ARCH=$(uname -m)
case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

rmdir /usr/lib/listaller
rmdir /usr/share/listaller
rm -f /usr/bin/lipa
rm -f /usr/bin/listallmgr-qt4
rm -f /usr/bin/listallmgr-gtk2

echo "Uninstalled."
