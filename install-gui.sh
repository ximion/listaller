#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
# DESTDIR		Destination root directory
# WIDGET                Widgetset the binary should be installed for
set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done

ARCH=$(uname -m)

case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

echo "Active widgetset: $WIDGET"

# Does the install

mkdir -p $DESTDIR/usr/bin
mkdir -p $DESTDIR/usr/share/
mkdir -p $DESTDIR/usr/share/applications/
mkdir -p $DESTDIR/usr/lib/listaller
if [ "$WIDGET" == "qt4" ]; then
mkdir -p $DESTDIR/usr/lib/listaller/qt4
else
mkdir -p $DESTDIR/usr/lib/listaller/gtk2
fi
#Copy other files
if [ "$WIDGET" == "qt4" ]; then
cp ./build/qt4/listallgo $DESTDIR/usr/lib/listaller/qt4/
cp ./build/qt4/listallmgr $DESTDIR/usr/lib/listaller/qt4/
cp ./build/qt4/liupdate $DESTDIR/usr/lib/listaller/qt4/
cp ./build/qt4/litray $DESTDIR/usr/lib/listaller/qt4/
else
cp ./build/gtk2/listallgo $DESTDIR/usr/lib/listaller/gtk2/
cp ./build/gtk2/listallmgr $DESTDIR/usr/lib/listaller/gtk2/
cp ./build/gtk2/liupdate $DESTDIR/usr/lib/listaller/gtk2/
cp ./build/gtk2/litray $DESTDIR/usr/lib/listaller/gtk2/
fi

if [ "$WIDGET" == "qt4" ]; then
cp ./data/applications/listaller-manager-kde.desktop $DESTDIR/usr/share/applications/
else
cp ./data/applications/listaller-manager-gnome.desktop $DESTDIR/usr/share/applications/
fi

#Create symlink
cd $DESTDIR/usr/bin
if [ "$WIDGET" == "qt4" ]; then
ln -s /usr/lib/listaller/qt4/listallmgr listallmgr-qt
else
ln -s /usr/lib/listaller/gtk2/listallmgr listallmgr-gtk
fi

echo "Installation done."

# ./install-cmds.sh
