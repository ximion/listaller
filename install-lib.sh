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

mkdir -p $DESTDIR/usr/lib

cp ./bin/libinstaller.so.0.4.0 $DESTDIR/usr/lib/
cd $DESTDIR/usr/lib/
ln -s /usr/lib/libinstaller.so.0.4.0 libinstaller.so.0.4

echo "Installation of libinstaller finished."
