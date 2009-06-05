#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
# DESTDIR		Destination root directory
# WIDGET                Widgetset the binary should be installed for
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
#
# "mkdir -p" is equivalent to ForceDirectories pascal function

mkdir -p $DESTDIR/usr/bin
mkdir -p $DESTDIR/usr/lib/listaller
if [ "$WIDGET" == "qt4" ]; then
mkdir -p $DESTDIR/usr/lib/listaller/qt4
else
mkdir -p $DESTDIR/usr/lib/listaller/gtk2
fi
#Copy other files
if [ "$WIDGET" == "qt4" ]; then
cp ./bin/qt4/listallgo $DESTDIR/usr/lib/listaller/qt4/
cp ./bin/qt4/listallmgr $DESTDIR/usr/lib/listaller/qt4/
cp ./bin/qt4/liupdate $DESTDIR/usr/lib/listaller/qt4/
else
cp ./bin/gtk2/listallgo $DESTDIR/usr/lib/listaller/gtk2/
cp ./bin/gtk2/listallmgr $DESTDIR/usr/lib/listaller/gtk2/
cp ./bin/gtk2/liupdate $DESTDIR/usr/lib/listaller/gtk2/
fi

if [ "$WIDGET" == "qt4" ]; then
cp "./additional/applications/listaller-manager-kde.desktop" $DESTDIR/usr/share/applications
else
cp "./additional/applications/listaller-manager-gnome.desktop" $DESTDIR/usr/share/applications
fi

#Create symlink
cd $DESTDIR/usr/bin
if [ "$WIDGET" == "qt4" ]; then
ln -s /usr/lib/listaller/qt4/listallmgr listallmgr-qt4
else
ln -s /usr/lib/listaller/gtk2/listallmgr listallmgr-gtk2
fi

echo "Installation done."
