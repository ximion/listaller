#!/bin/bash
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
rm -f /usr/bin/listallmgr

echo "Uninstalled."
