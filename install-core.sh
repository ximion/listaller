#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
# DESTDIR		Destination root directory

set -e

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
mkdir -p $DESTDIR/usr/bin
mkdir -p $DESTDIR/usr/sbin
mkdir -p $DESTDIR/usr/share
mkdir -p $DESTDIR/usr/share/dbus-1
mkdir -p $DESTDIR/usr/share/dbus-1/system-services/
mkdir -p $DESTDIR/etc/dbus-1/system.d/
mkdir -p $DESTDIR/usr/share/polkit-1/actions/

cp ./build/lipa $DESTDIR/usr/bin/
cp ./build/listallerd $DESTDIR/usr/sbin/
cp ./data/dbus/org.freedesktop.Listaller.service $DESTDIR/usr/share/dbus-1/system-services/
cp ./data/dbus/org.freedesktop.Listaller.conf $DESTDIR/etc/dbus-1/system.d/
cp ./data/dbus/org.freedesktop.listaller.policy $DESTDIR/usr/share/polkit-1/actions/

cp ./build/libinstaller.so.0.4.0 $DESTDIR/usr/lib/
cd $DESTDIR/usr/lib/
if [ ! -f libinstaller.so.0.4 ]; then
ln -s /usr/lib/libinstaller.so.0.4.0 libinstaller.so.0.4
fi

echo "Installation of core files finished."
