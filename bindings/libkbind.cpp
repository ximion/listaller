#include <stdio.h>
#include <stdlib.h>
#include <qbitmap.h>
#include <kiconloader.h>
#include <iostream>
 
extern "C" {
 
QBitmap Get_KDE_Icon(QString inm) 
{
 KComponentData ad;
 ad = KComponentData::KComponentData();
 KIconLoader *icl = KIconLoader::global();
 
 QPixmap icon = icl->loadIcon(QString(inm), KIconLoader::Desktop);
 return QBitmap(QPixmap);
}
 
}