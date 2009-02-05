#include <stdio.h>
#include <stdlib.h>
#include <qbitmap.h>
#include <kiconloader.h>
#include <iostream>
 
extern "C" {
 
QBitmap Get_KDE_Icon(char inm) 
{ 
 KIconLoader *icl = KIconLoader::global();
 
 QPixmap icon = icl->loadIcon(QString(inm), KIconLoader::Desktop);
 return QBitmap(icon);
}
 
}