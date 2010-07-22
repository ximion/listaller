#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
# DESTDIR		Destination root directory
# WIDGET                Widgetset the binary should be installed for
set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    WIDGET=*) WIDGET=${arg#WIDGET=};;
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

ARCH=$(uname -m)

case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

SDIR=$(pwd)
cd ..

echo "Active widgetset: $WIDGET"

# Does the install

mkdir -p $DESTDIR$prefix/bin
mkdir -p $DESTDIR$prefix/share/
mkdir -p $DESTDIR$prefix/share/applications/
mkdir -p $DESTDIR$libdir/listaller
if [ "$WIDGET" == "qt4" ]; then
mkdir -p $DESTDIR$libdir/listaller/qt4
else
mkdir -p $DESTDIR$libdir/listaller/gtk2
fi
#Copy other files
if [ "$WIDGET" == "qt4" ]; then
cp ./build/qt4/listallgo $DESTDIR$libdir/listaller/qt4/
cp ./build/qt4/listallmgr $DESTDIR$libdir/listaller/qt4/
cp ./build/qt4/liupdate $DESTDIR$libdir/listaller/qt4/
cp ./build/qt4/litray $DESTDIR$libdir/listaller/qt4/
else
cp ./build/gtk2/listallgo $DESTDIR$libdir/listaller/gtk2/
cp ./build/gtk2/listallmgr $DESTDIR$libdir/listaller/gtk2/
cp ./build/gtk2/liupdate $DESTDIR$libdir/listaller/gtk2/
cp ./build/gtk2/litray $DESTDIR$libdir/listaller/gtk2/
fi

if [ "$WIDGET" == "qt4" ]; then
cp ./data/applications/listaller-manager-kde.desktop $DESTDIR$prefix/share/applications/
else
cp ./data/applications/listaller-manager-gnome.desktop $DESTDIR$prefix/share/applications/
fi

#Create symlink
cd $DESTDIR$prefix/bin
if [ "$WIDGET" == "qt4" ]; then
if [ ! -f listallmgr-qt ]; then
ln -s $libdir/listaller/qt4/listallmgr listallmgr-qt
fi
else
if [ ! -f listallmgr-gtk ]; then
ln -s $libdir/listaller/gtk2/listallmgr listallmgr-gtk
fi
fi
cd $SDIR

echo "Installation done."

# ./install-cmds.sh
