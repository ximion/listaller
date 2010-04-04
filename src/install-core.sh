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

mkdir -p $DESTDIR$prefix/lib
mkdir -p $DESTDIR$prefix/bin
mkdir -p $DESTDIR$prefix/sbin
mkdir -p $DESTDIR$prefix/share
mkdir -p $DESTDIR$prefix/share/dbus-1/system-services/
mkdir -p $DESTDIR/etc/dbus-1/system.d/
mkdir -p $DESTDIR$prefix/share/polkit-1/actions/

cp ../build/lipa $DESTDIR$prefix/bin/
cp ../build/listallerd $DESTDIR$prefix/sbin/
cp ../data/org.nlinux.Listaller.service $DESTDIR$prefix/share/dbus-1/system-services/
cp ../data/org.nlinux.Listaller.conf $DESTDIR/etc/dbus-1/system.d/
cp ../data/org.nlinux.listaller.policy $DESTDIR$prefix/share/polkit-1/actions/

#Other required data files
mkdir -p $DESTDIR/etc/lipa
mkdir -p $DESTDIR/etc/lipa/app-reg
cp ../data/blacklist $DESTDIR/etc/lipa/
cp ../data/ignore-deps.list $DESTDIR/etc/lipa/

cp ../build/libinstaller.so.0.4.0 $DESTDIR$prefix/lib/
cd $DESTDIR$prefix/lib/
if [ ! -f libinstaller.so.0.4 ]; then
ln -s /usr/lib/libinstaller.so.0.4.0 libinstaller.so.0.4
fi

echo "Installation of core files finished."
