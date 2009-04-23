#!/usr/bin/env bash
#
# DESTDIR		Destination root directory
# WIDGET                Widgetset the binary should be installed for
for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done

# Detects and parses the architecture
ARCH=$(uname -m)

case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac

echo "Target architecture: $ARCH"

if [ $ARCH = "x86_64" ]; then LCLDir="/usr/lib64/lazarus/"
else LCLDir="/usr/lib/lazarus"
fi
echo "LAZTarget: $LCLDir"

OS="linux"

echo "Target operating system: $OS"

# Command line to build the sofware
# Create the binary dir
# Create necessary dirs
mkdir -p ./bin
mkdir -p ./bin/lang
mkdir -p ./bin/qt4
mkdir -p ./bin/gtk2

VER=$(fpc -iW)
echo "Compiler version: $VER"
echo "LAZBuild version: $($LCLDir/lazbuild -v)"

echo "Creating installer..."
if [ "$WIDGET" == "qt4" ]; then
$LCLDir/lazbuild -B --ws=qt4 listallgo.lpr
cp ./bin/listallgo ./bin/qt4/
else
$LCLDir/lazbuild -B --ws=gtk2 listallgo.lpr
cp ./bin/listallgo ./bin/gtk2/
fi
echo "Creating software-manager..."
if [ "$WIDGET" == "qt4" ]; then
$LCLDir/lazbuild -B --ws=qt4 listallmngr.lpr
cp ./bin/listallmgr ./bin/qt4/
else
$LCLDir/lazbuild -B --ws=gtk2 listallmngr.lpr
cp ./bin/listallmgr ./bin/gtk2/
fi
echo "Creating updater..."
if [ "$WIDGET" == "qt4" ]; then
$LCLDir/lazbuild -B --ws=qt4 liupdate.lpr
cp ./usr/liupdate ./usr/qt4/
else
$LCLDir/lazbuild -B --ws=gtk2 liupdate.lpr
cp ./usr/liupdate ./usr/gtk2/
fi
echo "Creating command-line tool..."
fpc -MObjFPC -Sgi -CX -b -O1 -gl -XX -vewnhi -l -dOpbCompat -Fuopbitmap/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -obin/lipa lipa.lpr
echo "Creating unified build tool..."
fpc -MObjFPC -Sgi -CX -b -O1 -gl -XX -vewnhi -l -dOpbCompat -Fuopbitmap/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -obin/unibuild unibuild.lpr