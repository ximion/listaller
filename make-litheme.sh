#!/usr/bin/env bash
# WIDGET                Widgetset the binary should be installed for
set -e

for arg; do
  case $arg in
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done
# Create necessary dirs
mkdir -p ./bin
mkdir -p ./bin/gtk2
mkdir -p ./bin/qt4
cd ./liThemeHandler
./make.sh "DESTDIR=$DESTDIR"
