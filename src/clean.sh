#!/bin/bash
# Clean up all files which were used during the compile process or which are just backup files

for arg; do
  case $arg in
    ALL=*) ALL=${arg#ALL=};;
  esac;
done

if [ "$ALL" == "1" ]; then
 rm -f Makefile
fi

rm -f ./libinstaller.so
rm -f ./libinstaller.so.0.4

if [ "$ALL" == "1" ]; then
 rm -f Makefile
fi

cd ..

rm -rf ./build

find -iname *.ppu -exec rm {} \;
find -iname *.o -exec rm {} \;
find -iname *.or -exec rm {} \;
find -iname *.res -exec rm {} \;
find -iname *.a -exec rm {} \;
find -iname *.pas~ -exec rm {} \;
find -iname *.bak -exec rm {} \;

find -iname *.compiled -exec rm {} \;
find -iname *.manifest -exec rm {} \;
find -iname *.lrs -exec rm {} \;

find -iname *~ -exec rm {} \;

cd ./src

echo "Source code directories cleaned up."
