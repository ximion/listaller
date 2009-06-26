#!/usr/bin/env bash
# Clean up all files which were used during the compile process or which are just backup files

rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak

rm -rf *.compiled
rm -rf libgtk.so
rm -rf libgdk_pixbuf.so
rm -rf libglib.so
rm -rf libgdk.so
rm -rf *.lrt

rm -rf *~
rm -rf "./bin"

#Clean liCreator directory
cd ./liCreator/
rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak

rm -rf *.compiled
rm -rf libgtk.so
rm -rf libgdk_pixbuf.so
rm -rf libglib.so
rm -rf libgdk.so

rm -rf *~

cd ..
#Clean liThemeHandler directory
cd ./liThemeHandler/
rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak

rm -rf *.compiled
rm -rf libgtk.so
rm -rf libgdk_pixbuf.so
rm -rf libglib.so
rm -rf libgdk.so

rm -rf *~
