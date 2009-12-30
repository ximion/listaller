#!/bin/sh
# Clean up all files which were used during the compile process or which are just backup files

rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak

rm -rf *.compiled
rm -rf *.lrt
rm -rf *.lrs
rm -f Makefile

rm -rf *~
rm -rf "../build"

rm -f ./libinstaller.so
rm -f ./libinstaller.so.0.4
rm -f ./libappmanager.so

#Clean liCreator directory
cd ../licreator
rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak
rm -rf *.lrs

rm -rf *.compiled

rm -rf *~

cd ..

#Clean root dir
rm -f Makefile
rm -rf *~
rm -rf *.bak

#Clean libs directory
cd ./lib
rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak

rm -rf *.compiled
rm -rf *.lrs

rm -rf *~

cd ..

#Clean helper daemon directory
cd ./helper
rm -rf *.o
rm -rf *.ppu

rm -rf *.pas~
rm -rf *.sh~
rm -rf *.bak

rm -rf *.compiled
rm -rf *.lrs

rm -rf *~

cd ../src

echo "Source code directories cleaned up."
