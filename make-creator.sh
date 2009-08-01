#!/usr/bin/env bash
#
# WIDGET                Widgetset the binary should be installed for
set -e

for arg; do
  case $arg in
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done
# Read system architecture
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
#Workaround for LazPackaging bug
LCLDir="/usr/lib/lazarus"

OS="linux"

#Create necessary directories
mkdir -p ./bin
mkdir -p ./bin/gtk2
mkdir -p ./bin/qt4
echo "Target operating system: $OS"
cd ./liCreator
./make.sh "WIDGET=$WIDGET"
