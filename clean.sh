#!/usr/bin/env bash
# Clean up all files which were used during the compile process or which are just backup files

rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak

rm -rf *.compiled
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

rm -rf *~

cd ..

#Clean libs directory
cd ./libs/
rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak

rm -rf *.compiled

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

rm -rf *~
