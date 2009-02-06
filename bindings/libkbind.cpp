#include <stdio.h>
#include <stdlib.h>
#include <qbitmap.h>
#include <kiconloader.h>
#include <iostream>
 
//extern "C" {
 
QBitmap Get_KDE_Icon(QString inm) 
{ 
 KIconLoader *icl = KIconLoader::global();
 
 QPixmap icon = icl->loadIcon(QString(inm), KIconLoader::Desktop);
 icon.save("./test.png","PNG",0);
}

int main()
{
 Get_KDE_Icon("akonadi");
}
 
//}