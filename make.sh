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

LCLDir="/usr/lib/lazarus"
#Libuild needed because lazbuild does not work
chmod +x ./libuild
#

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

#Workaround for Lazarus-Alpha-packaging bug
#echo "LAZBuild version: $($LCLDir/lazbuild -v)"

echo "Creating installer..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=listallgo.lpr O=listallgo WIDGET=qt4
$LCLDir/lazbuild -r --ws=qt4 listallgo.lpr
mv ./bin/listallgo ./bin/qt4/
else
#./libuild PR=listallgo.lpr O=listallgo WIDGET=gtk2
$LCLDir/lazbuild -r --ws=gtk2 listallgo.lpr
mv ./bin/listallgo ./bin/gtk2/
fi
echo "Creating software-manager..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=listallmngr.lpr O=listallmgr WIDGET=qt4
$LCLDir/lazbuild -r --ws=qt4 listallmngr.lpr
mv ./bin/listallmgr ./bin/qt4/
else
#./libuild PR=listallmngr.lpr O=listallmgr WIDGET=gtk2
$LCLDir/lazbuild -r --ws=gtk2 listallmngr.lpr
mv ./bin/listallmgr ./bin/gtk2/
fi
echo "Creating updater..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=liupdate.lpr O=liupdate WIDGET=qt4
$LCLDir/lazbuild -B --ws=qt4 liupdate.lpr
mv ./bin/liupdate ./bin/qt4/
else
#./libuild PR=liupdate.lpr O=liupdate WIDGET=gtk2
$LCLDir/lazbuild -B --ws=gtk2 liupdate.lpr
mv ./bin/liupdate ./bin/gtk2/
fi

#Also workarount for lazbuild bug in Lazarus-Alpha
WIDGET="gtk2"

echo "Creating command-line tool..."
fpc -MObjFPC -Sgi -CX -O1 -gl -XX -vewhi -l -Fuopbitmap/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu. -FUbin/ -FEbin/ -olipa -dOpbCompat lipa.lpr
echo "Creating unified build tool..."
fpc  -MObjFPC -Sgi -CX -O1 -gl -XX -vewnhi -l -Fuopbitmap/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu. -FUbin/ -FEbin/ -ounibuild -dOpbCompat unibuild.lpr
#-Fi$LCLDir/lcl/include/
