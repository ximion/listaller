#!/usr/bin/env bash
# Parses command line options. Currently supported options are:
#
# DESTDIR		Destination root directory
# WIDGET                Widgetset the binary should be installed for
for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done

# Does the install
#
# "mkdir -p" is equivalent to ForceDirectories pascal function
#
echo "Installation of 'liThemeMgr'"

mkdir -p $DESTDIR/usr/bin/
mkdir -p $DESTDIR/usr/appfiles/
mkdir -p $DESTDIR/usr/appfiles/liThemeMgr
mkdir -p $DESTDIR/usr/appfiles/liThemeMgr/graphics
mkdir -p $DESTDIR/usr/share/applications

cp './graphics/ligraphic-header.png' $DESTDIR/usr/appfiles/liThemeMgr/graphics
cp './graphics/spackage.png' $DESTDIR/usr/appfiles/liThemeMgr/graphics

if [ "$WIDGET" == "qt4" ]; then
cp './bin/qt4/litheme' $DESTDIR/usr/appfiles/liThemeMgr/
else
cp './bin/gtk2/litheme' $DESTDIR/usr/appfiles/liCreator/
fi
cp "./liThemeHandler/liThemeMgr.desktop" $DESTDIR"/usr/share/applications/"

#Create symlink
cd $DESTDIR/usr/bin
ln -s /usr/appfiles/liThemeMgr/litheme litheme

echo "Installation done. (liThemeMgr)"
