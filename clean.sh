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

rm -f ./libinstaller.so
rm -f ./libinstaller.so.0.4
rm -f ./libappmanager.so

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
