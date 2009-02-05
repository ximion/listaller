#include <kglobal.h>
#include <kiconloader.h>
#include <qobject>
#include <iostream>
 
//g++ libkpixmap.cpp -I/usr/lib/kde4/include -I/usr/include/qt4/Qt/ -I/usr/include/qt4/QtCore/ -c
//g++ -shared -Wl,-soname,libkicon.so -o libkicon.so.1.0.0 sharedl.o -L/usr/lib/kde4/lib -L$QTDIR/lib -lkparts -lkdecore -lkio
extern "C" {
 
 
QPixmap* Get_KDE_Icon(char* inm) 
{
 KIconLoader *loader = KIconLoader* KIconLoader::global();

 QPixmap icon;
 icon = loader->loadIcon(QString(inm), KIconLoader::Toolbar);
 return icon;
}
 
}