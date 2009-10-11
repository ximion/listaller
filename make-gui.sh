#!/usr/bin/env bash
#
# DESTDIR		Destination root directory
# WIDGET                Widgetset the binary should be installed for
set -e

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

if [ "$ARCH" == "x86_64" ]; then
LCLDir="/usr/lib64/lazarus"
else
LCLDir="/usr/lib/lazarus"
fi

echo "LAZTarget: $LCLDir"

OS="linux"

echo "Target operating system: $OS"
echo "Active widgetset: $WIDGET"

# Command line to build the sofware
# Create necessary dirs
mkdir -p ./bin
mkdir -p ./bin/locale
mkdir -p ./bin/qt4
mkdir -p ./bin/gtk2

VER=$(fpc -iW)
echo "Compiler version: $VER"

echo "Compiling installer..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=listallgo.lpr O=listallgo WIDGET=qt4
lazbuild -B --ws=qt listallgo.lpr
mv ./bin/listallgo ./bin/qt4/
else
#./libuild PR=listallgo.lpr O=listallgo WIDGET=gtk2
lazbuild -B --ws=gtk2 listallgo.lpr
mv ./bin/listallgo ./bin/gtk2/
fi
echo "Compiling software-manager..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=listallmngr.lpr O=listallmgr WIDGET=qt4
lazbuild -B --ws=qt listallmgr.lpr
mv ./bin/listallmgr ./bin/qt4/
else
#./libuild PR=listallmngr.lpr O=listallmgr WIDGET=gtk2
lazbuild -B --ws=gtk2 listallmgr.lpr
mv ./bin/listallmgr ./bin/gtk2/
fi
echo "Compiling updater..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=liupdate.lpr O=liupdate WIDGET=qt4
lazbuild -B --ws=qt liupdate.lpr
mv ./bin/liupdate ./bin/qt4/
else
#./libuild PR=liupdate.lpr O=liupdate WIDGET=gtk2
lazbuild -B --ws=gtk2 liupdate.lpr
mv ./bin/liupdate ./bin/gtk2/
fi
echo "Compiling tray notifier..."
if [ "$WIDGET" == "qt4" ]; then
#./libuild PR=liupdate.lpr O=liupdate WIDGET=qt4
lazbuild -B --ws=qt litray.lpr
mv ./bin/litray ./bin/qt4/
else
#./libuild PR=liupdate.lpr O=liupdate WIDGET=gtk2
lazbuild -B --ws=gtk2 litray.lpr
mv ./bin/litray ./bin/gtk2/
fi

echo "Listaller GUI build completed. (For $WIDGET)"
