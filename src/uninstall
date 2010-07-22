#!/bin/bash
# Removes all files which are installed by listaller-install
set -e

for arg; do
  case $arg in
    prefix=*) prefix=${arg#prefix=};;
    libdir=*) libdir=${arg#libdir=};;
  esac;
done

if [ -z "$prefix" ]; then
   export prefix="/usr"
fi

echo "Uninstallation started."
ARCH=$(uname -m)
case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

rm -rf $libdir/listaller
rm -f $libdir/libinstaller.so
rm -f $libdir/libinstaller.so.*
rm -rf $prefix/share/listaller
rm -f $prefix/bin/lipa
rm -f $prefix/bin/listallmgr-qt4
rm -f $prefix/bin/listallmgr-gtk2

echo "Uninstalled."
