/*
* Copyright (C) 2010 Matthias Klumpp
*
* Authors:
*  Filippos Papadopoulos
*  Matthias Klumpp
*
* This unit is free software: you can redistribute it and/or modify it under
* the terms of the GNU General Public License as published by the Free Software
* Foundation, version 3.
*
* This unit is distributed in the hope that it will be useful, but WITHOUT
* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
* FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License v3
* along with this library. If not, see <http://www.gnu.org/licenses/>.
*/
#include "mainwindow.h"
#include "ui_mainwindow.h"
#include <iostream>
#include "treemodel.h"
#include "treeitem.h"

using std::vector;
using std::string;
using std::cout;
using std::cerr;
using std::endl;

//this is declared and specified in read_elf.cpp. It checks if the given filename is a valid ELF
extern int processFile(char *, QWidget*);

extern vector<unsigned int> displayError;
vector <QString> neededLibVector;
vector <QString> rpathVector;

MainWindow::MainWindow(QWidget *parent) :
        QMainWindow(parent),
        ui(new Ui::MainWindow)
{
    char* needed = (char*) calloc(512, 1);
    char* tmp = (char*) calloc(512, 1);
    char* buf = (char*) calloc(512, 1);

    neededLibVector.reserve(10);
    vectorLdConfig.reserve(1000);


    // We add to the vectorLdConfig the output of the '/sbin/ldconfig -p' command
    // That means we only add the ldconfig cache. Later on we add libraries that we may find in the current path.
    FILE* stream_ldconf = popen("/sbin/ldconfig -p", "r");
    if(stream_ldconf==NULL)
        perror("popen_ldconf");

    while(!feof(stream_ldconf))
    {
        memset(tmp, 0, 512);
        fgets(tmp, 512, stream_ldconf);
        if(!feof(stream_ldconf))
        {
            /* libqt-mt.so.3 (libc6) => /usr/lib/libqt-mt.so.3 */
            /* libc.so.6 (libc6, OS ABI: Linux 2.4.0) => /lib/libc.so.6 */
            // 			cout << "QString(tmp)==" << QString(tmp) << endl;
            vectorLdConfig.push_back(QString(tmp));
        }
    }
    pclose(stream_ldconf);

    free(needed);
    free(tmp);
    free(buf);

    ui->setupUi(this);

    statusLabel = new QLabel("No file loaded", 0);
    statusBar()->addWidget(statusLabel);
}

MainWindow::~MainWindow()
{
    delete ui;
}

/* Searches for *.so*  in the current path (dirname)
 * and adds them to the vectorLdConfig
 */
void MainWindow::traverse(const QString& dirname)
{
        QDir dir(dirname, "*.so*");
        dir.setFilter(QDir::Files);
        QFileInfoList fileinfolist = dir.entryInfoList();
        QListIterator<QFileInfo> it(fileinfolist);

        QString string;


        while (it.hasNext())
        {
             QFileInfo fi = it.next();
             if(fi.fileName() == "." || fi.fileName() == "..")
             {
                     continue;
             }

             if(fi.isDir() && fi.isReadable())
                     ;
             else
             {
                     string = QString(fi.fileName());
                     vectorLdConfig.push_back((string.append(" => ")).append(fi.absoluteFilePath()));
             }
        }

        return;
}

QString MainWindow::findFullPath(QString soname)
{
   QString txt;
   int index = 0;

   /* for each library name we must find the full path in vector ldconfig
   The 'soname' looks like libcrypto.so.0.9.6 */
   for(unsigned int i=0; i < vectorLdConfig.size(); i++)
   {
      if(vectorLdConfig[i].indexOf(soname)!=-1)
      {
         index = vectorLdConfig[i].indexOf("/");
         if(index != -1)
         {
                                             //finally we remove /n
            return vectorLdConfig[i].mid(index).remove(QChar('\n'));
         }
         else
            cerr  << "_________________________INDEX = -1\n";
      }
   }
   /* if the execution reaches here it means that the library with 'soname' as its name, was not found
      inside ld's cache. Thus we look for RPATH existance */

   //if rpath is found then rpathVector will have at least one item
   //inside RPATH, directory names are separated with an ':' (Solaris only???)
   if(rpathVector.size() > 0)
   {
      return rpathVector[0] + "/" + soname;
   }


   // Have a look in the current directory for the library
   txt = lastFileName.left(lastFileName.lastIndexOf("/")) + "/" + soname;
   if (QFile::exists(txt))
   {
      return txt;
   }

   // Maybe it exists with a slightly different name
   QDir dir(lastFileName.left(lastFileName.lastIndexOf("/")));
   QStringList entries = dir.entryList(QDir::Files, QDir::Name);
   entries = entries.filter(QRegExp(soname + "*"));

   for ( QStringList::Iterator it = entries.begin(); it != entries.end(); ++it )
   {
      return lastFileName.left(lastFileName.lastIndexOf("/")) + "/" + (*it);
   }

   cout << "Library " << qPrintable(soname) << " could not be found." << endl;  // we could not find the needed library...
   return QString::null;
}

void MainWindow::loadFile(QString filename)
{

    if(filename.isEmpty())
        return;

    char *tmp = (char *) calloc(512, 1);
    int ret = filename.lastIndexOf("/");
    QString currentPath = filename.left(ret); // Strip off the filename, so we get just a directory
    if(ret == -1)
        currentPath = "/";


    lastFileName = filename;
    statusBar()->clearMessage();

    neededLibVector.clear();
    //we add items to the neededLibVector inside the process_file function
    int res = processFile(strcpy(tmp, filename.toAscii()), this);   // we check if the given filename is a valid ELF
    if(res == 0)
        statusLabel->setText("File " + filename + " loaded");
    else
        statusLabel->setText(" ");

    traverse(currentPath);

    TreeModel *model = new TreeModel(this);
    ui->treeView->setModel(model);

    //for each library on the 'first level'
    for(unsigned int i=0; i < neededLibVector.size(); i++)
    {
        QString fullPath, dirname;
        QString soname(neededLibVector[i].toAscii());
        fullPath = findFullPath (soname);
        dirname = fullPath.left(fullPath.lastIndexOf("/"));
        //string soname contains only the name of the lib. dirname contains the lib's path
        model->getRoot()->appendChild(new TreeItem(soname, dirname));
        //model->getRoot()->appendChild(item);
    }

   /* TreeItem *parentItem = model->getRoot();
    //the first root
     //QTreeWidgetItem *myChild = ui->treeView->;
    while(myChild)
    {
        //the resolver function called for each root
        resolver(myChild);
        myChild = dynamic_cast <booleanListViewItem*> (myChild->nextSibling());
    }

    //after we scan the whole list we then delete the not-found libraries
    for(unsigned int i=0; i < deletedVector.size(); i++)
    {
        delete deletedVector[i];
    }

    deletedVector.clear(); */
    free(tmp);

    return;
}

void MainWindow::fileOpen()
{
    QString startingPath = "/";

    if (!lastFileName.isEmpty())
    {
        // Strip off the filename, so we get just a directory
        startingPath = lastFileName.left(lastFileName.lastIndexOf("/"));
    }
    QString filename = QFileDialog::getOpenFileName(this, "Select file to analyze...", startingPath, QString::null);

    loadFile(filename);

    return;
}
