#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
# DESTDIR		Destination root directory

#set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
  esac;
done

ARCH=$(uname -m)
case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

# Does the install

mkdir -p $DESTDIR/usr/bin
mkdir -p $DESTDIR/usr/lib

cp ./bin/lipa $DESTDIR/usr/bin/
cp ./bin/libinstaller.so.0.4 $DESTDIR/usr/lib/

echo "Installation done."
