#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
# DESTDIR		Destination root directory
set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    prefix=*) prefix=${arg#prefix=};;
  esac;
done

if [ -z "$prefix" ]; then
   export prefix="/usr"
fi

ARCH=$(uname -m)
case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

# Does the install
#
# "mkdir -p" is equivalent to ForceDirectories pascal function

mkdir -p $DESTDIR$prefix/share/listaller
mkdir -p $DESTDIR$prefix/share/listaller/graphics
mkdir -p $DESTDIR$prefix/share/listaller/graphics/libutton
mkdir -p $DESTDIR$prefix/bin

cp ./build/libuild $DESTDIR$prefix/bin/

#Copy graphics
cp ./graphics/libutton/left.png $DESTDIR$prefix/share/listaller/graphics/libutton/
cp ./graphics/libutton/firstblock.png $DESTDIR$prefix/share/listaller/graphics/libutton/
cp ./graphics/libutton/block.png $DESTDIR//usr/share/listaller/graphics/libutton/
cp ./graphics/libutton/lastblock.png $DESTDIR$prefix/share/listaller/graphics/libutton/
cp -dpr ./graphics/libutton/distro/ $DESTDIR$prefix/share/listaller/graphics/libutton/distro/
rm -rf $DESTDIR$prefix/share/listaller/graphics/libutton/distro/.svn
rm -rf $DESTDIR$prefix/share/listaller/graphics/libutton/distro/.directory

echo "Installation done."
