#ifndef AUTOPACKAGE_LDD_SUB_H
#define AUTOPACKAGE_LDD_SUB_H

#include <QtGui>
#include <stdio.h>
#include <unistd.h>
#include <string>
#include <iostream>
#include <vector>
#include <algorithm>
#include "ui_autopackage_ldd.h"


class autopackage_ldd_Sub : public Ui_autopackage_ldd
{
   Q_OBJECT
   
   std::vector <QString> vectorLdConfig;
   QLabel *statusLabel;
   
   public:
       autopackage_ldd_Sub( QWidget* parent = 0, const char* name = 0, QString argFilename = 0);
       ~autopackage_ldd_Sub();

   //void resolver(booleanListViewItem* itemChild);
   QString findFullPath(QString soname);
   void loadFile(QString filename);
	void traverse(const QString& dirname);
   
	public slots:
       void fileOpen();
		 void aboutMenuItemSlot();
		 void aboutQtMenuItemSlot();
       //void findChildSlot(QListViewItem *);          /* called when an expansion of a root is made */
       //void deleteChildrenSlot(QListViewItem *);    /* called when a collapse of a root is made */
       //void rightButtonPressed(QListViewItem *, const QPoint &, int );
       void expandAll();
       void collapseAll();
       void dumpTree();
       
   private:
       QString lastFileName;
};

#endif // AUTOPACKAGE_LDD_SUB_H
