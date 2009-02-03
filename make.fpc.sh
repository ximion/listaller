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

echo "Creating installer..."
fpc  -MObjFPC -Sgi -CX -O1 -gl -XX -WG -vewnhi -l -Fusynapse/ -Fu$LCLDir/components/synedit/ -Fu$LCLDir/components/synedit/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -obin/listallgo -dLCL -dLCLgtk2 listallgo.lpr
# fpc -S2cgi -OG1 -gl -CX -XX -WG -vewnhi -l -Fusynapse/ -Fu$LCLDir/components/synedit -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -obin/listallgo -dLCL -dLCLgtk2 listallgo.lpr
echo "Creating software-manager..."
fpc -S2cgi -OG1 -gl -CX -XX -WG -vewnhi -l -Fusynapse/ -Fu$LCLDir/components/synedit/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -obin/listallmgr -dLCL -dLCLgtk2 listallmngr.lpr
echo "Creating updater..."
fpc -S2cgi -OG1 -gl -CX -XX -WG -vewnhi -l -Fusynapse/ -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -obin/liupdate -dLCL -dLCLgtk2 liupdate.lpr
echo "Creating command-line tool..."
fpc -S2cgi -OG1 -gl -CX -XX -vewnhi -l -Fu$LCLDir/lcl/units/$ARCH-$OS/ -Fu$LCLDir/lcl/units/$ARCH-$OS/gtk2/ -Fu$LCLDir/components/codetools/units/$ARCH-$OS/ -Fu$LCLDir/components/custom/ -Fu$LCLDir/packager/units/$ARCH-$OS/ -Fu. -obin/lipa lipa.lpr
