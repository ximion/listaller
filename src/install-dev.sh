#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
#
# DESTDIR		Destination root directory
set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    prefix=*) prefix=${arg#prefix=};;
    libdir=*) libdir=${arg#libdir=};;
  esac;
done

if [ -z "$prefix" ]; then
   export prefix="/usr"
fi
if [ -z "$libdir" ]; then
   export libdir="$prefix/lib"
fi

echo "Installing development files..."
mkdir -p $DESTDIR$prefix/include/Listaller
mkdir -p $DESTDIR$libdir/pkgconfig/

#Copy C headers
cp ../lib/cbind/*.h $DESTDIR$prefix/include/Listaller/
sed "s#%PREFIX%#$prefix#" ../lib/cbind/libinstaller.pc.in > ../lib/cbind/libinstaller.pc
cp ../lib/cbind/*.pc $DESTDIR$libdir/pkgconfig/
cd $DESTDIR$libdir
if [ ! -f libinstaller.so ]; then
ln -s $libdir/libinstaller.so.0.4 libinstaller.so
fi

echo "Devfiles installed."
