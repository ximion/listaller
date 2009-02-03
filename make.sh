#!/usr/bin/env bash
#
# Detects and parses the architecture
#

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
# Building the IDE and LCL
#
# Detects and parses the OS
#

OS="linux"

echo "Target operating system: $OS"

#
# Command line to build the sofware
#
# Create the binary dir

mkdir ./bin
mkdir ./bin/lang

VER=$(fpc -iW)
echo "Compiler version: $VER"
echo "LAZBuild version: $($LCLDir/lazbuild -v)"

echo "Creating installer..."
$LCLDir/lazbuild -B --ws=gtk2 listallgo.lpr
echo "Creating software-manager..."
$LCLDir/lazbuild -B --ws=gtk2 listallmngr.lpr
echo "Creating updater..."
$LCLDir/lazbuild -B --ws=gtk2 liupdate.lpr
echo "Creating command-line tool..."
$LCLDir/lazbuild -B --ws=gtk2 lipa.lpr
echo "Creating unified build tool..."
$LCLDir/lazbuild -B --ws=gtk2 unibuild.lpr