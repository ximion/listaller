#include "autopackage_ldd_Sub.h"

#include <qapplication.h>

//this is declared and specified in read_elf.cpp. It checks if the given filename is a valid ELF
extern int process_file(char *, autopackage_ldd_Sub*);

using std::vector;
using std::string;
using std::cout;
using std::cerr;
using std::endl;

extern vector<unsigned int> displayError;
vector <QString> neededLibVector;
vector <QString> rpathVector;
vector <QListViewItem*> deletedVector;



/* 
 *  Constructs a autopackage_ldd_Sub which is a child of 'parent', with the 
 *  name 'name' and widget flags set to 'f' 
 * We initialize some things here...
*/
autopackage_ldd_Sub::autopackage_ldd_Sub( QWidget* parent,  const char* name, QString argFilename)
    : autopackage_ldd( parent, name, fl )
{
   char* needed = (char*) calloc(512, 1);
   char* tmp = (char*) calloc(512, 1);
   char* buf = (char*) calloc(512, 1);
   
	neededLibVector.reserve(10);
   vectorLdConfig.reserve(1000);


	/* We add to the vectorLdConfig the output of the '/sbin/ldconfig -p' command 
	* That means we only add the ldconfig cache. Later on we add libraries that we may find in the current path.
	*/
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
	   
   statusLabel = new QLabel("No file loaded", 0);   
   statusBar()->addWidget(statusLabel);   
   
   free(needed);
   free(tmp);
   free(buf);
   if(argFilename != 0)
      loadFile(argFilename);
      
   connect(listView, SIGNAL(rightButtonPressed(QListViewItem *, const QPoint &, int )), SLOT(rightButtonPressed(QListViewItem *, const QPoint &, int )));
}


	/* Destructor */
autopackage_ldd_Sub::~autopackage_ldd_Sub()
{
    // no need to delete child widgets, Qt does it all for us
}



void autopackage_ldd_Sub::rightButtonPressed(QListViewItem *, const QPoint &p, int )
{
   QPopupMenu* menu = new QPopupMenu(this);
   menu->insertItem("Expand all", this, SLOT(expandAll()));
   menu->insertItem("Collapse all", this, SLOT(collapseAll()));
   menu->insertSeparator();
   menu->insertItem("Dump tree to file...", this, SLOT(dumpTree()));
   menu->exec(p);
}

void autopackage_ldd_Sub::expandAll()
{
   int c = 0;
   QListViewItem* i = listView->firstChild();
   while (i != 0)
   {
      if (i->childCount() > 0)
         i->setOpen(true);
      i = i->itemBelow();
      c++;
      if (c % 100 == 0) // Only update the UI every 100 items
      {
         statusBar()->message("Expanding all items... " + QString::number(c) );
         qApp->processEvents();
      }
   }
   statusBar()->clear();
}

void autopackage_ldd_Sub::collapseAll()
{
   QListViewItem* i = listView->firstChild();
   while (i != 0)
   {
      i->setOpen(false);
      i = i->itemBelow();
   }
}

void autopackage_ldd_Sub::dumpTree()
{
   QString fileName = QFileDialog::getSaveFileName(QDir::homeDirPath(), QString::null, this, "", "Save tree...");
	//first we check if the user press Cancel 
	if(fileName.isEmpty())
		return;
   
	QFile file(fileName);
   file.open(IO_WriteOnly);
   QTextStream stream(&file);

   int c = 0;
   QListViewItem* i = listView->firstChild();
   while (i != 0)
   {
      if (i->childCount() > 0)
         i->setOpen(true);
      
      QString spaces;
      spaces.fill(' ', 1 + i->depth()*2);
		stream << spaces << i->text(0) << spaces << i->text(1) << '\n';
      
      i = i->itemBelow();
      c++;
      if (c % 100 == 0) // Only update the UI every 100 items
      {
         statusBar()->message("Saving... " + QString::number(c) );
         qApp->processEvents();
      }
   }
   statusBar()->message("Saved tree to " + fileName);
   
   file.close();
}



void autopackage_ldd_Sub::resolver(booleanListViewItem* itemChild)
{
   if(itemChild==NULL)
   {
      return;
   }
   
   int res = 1;
	
   QString fullPath = findFullPath(itemChild->text(0));
	char* tmp;
   
   if (!fullPath.isEmpty())
   {
      tmp = (char*) malloc(fullPath.length() + 1);
      strcpy(tmp, fullPath.latin1());
   }
   else
   {
      tmp = (char*) malloc(1);
      tmp[0] = '\0';
   }
   
   neededLibVector.clear();
   res = process_file(tmp, this);
   
   if(res == 1 && neededLibVector.size() == 0) //the name  was not found
   {   
      if(itemChild->parent() == 0) //NULL
         new myListViewItem(listView, itemChild->text(0), "not found");     
      else
			new myListViewItem(itemChild->parent(), itemChild->text(0), "not found");     
      
      deletedVector.push_back(itemChild); // we hold in a vector the items that will be deleted
   }
   else
      for(unsigned int i=0; i < neededLibVector.size(); i++)
      {   
			QString dirname;
			QString soname(neededLibVector[i].ascii());
			fullPath = findFullPath (soname);
			dirname = fullPath.left(fullPath.findRev("/"));
         new booleanListViewItem(itemChild, soname, dirname);     
      }

      
   free(tmp);
}






QString autopackage_ldd_Sub::findFullPath(QString soname)
{
   QString txt;
   int index = 0;
   
   /* for each library name we must find the full path in vector ldconfig 
   The 'soname' looks like libcrypto.so.0.9.6 */
   for(unsigned int i=0; i < vectorLdConfig.size(); i++)
   {   
      if(vectorLdConfig[i].find(soname)!=-1)   
      {
         index = vectorLdConfig[i].find("/");
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
   txt = lastFileName.left(lastFileName.findRev("/")) + "/" + soname;
   if (QFile::exists(txt))
   {
      return txt;
   }
   
   // Maybe it exists with a slightly different name
   QDir dir(lastFileName.left(lastFileName.findRev("/")));
   QStringList entries = dir.entryList(soname + "*");
   for ( QStringList::Iterator it = entries.begin(); it != entries.end(); ++it )
   {
      return lastFileName.left(lastFileName.findRev("/")) + "/" + (*it);
   }
   
   cerr << "Library " << soname << " could not be found\n";  // we could not find the needed library...
   return QString::null;
}





void autopackage_ldd_Sub::fileOpen()
{
   QString startingPath = "/";
   
   if (!lastFileName.isEmpty())
   {
        // Strip off the filename, so we get just a directory
        startingPath = lastFileName.left(lastFileName.findRev("/"));
   }
	QString filename = QFileDialog::getOpenFileName(startingPath, QString::null, this);
   
	loadFile(filename);
   
	return;
}



/* Searches for *.so*  in the current path (dirname) 
* and adds them to the vectorLdConfig 
*/
void autopackage_ldd_Sub::traverse(const QString& dirname)
{
	QDir dir(dirname, "*.so*");
	dir.setFilter(QDir::Files);
	const QFileInfoList* fileinfolist = dir.entryInfoList();
	QFileInfoListIterator it(*fileinfolist);
	QFileInfo* fi = 0;
	QString string; 
	
	
	while((fi=it.current()))
	{
		if(fi->fileName() == "." || fi->fileName() == "..")
		{
			++it;
			continue;
		}
		
		if(fi->isDir() && fi->isReadable())
			;
		else
		{
			string = QString(fi->fileName());
			
 			vectorLdConfig.push_back((string.append(" => ")).append(fi->absFilePath()));
// 			qDebug("%s", string.ascii());
		}
		
		++it;
	}	
	
	return;	
}



void autopackage_ldd_Sub::loadFile(QString filename)
{
   
	if(filename.isEmpty())
		return;
   
	char *tmp = (char *) calloc(512, 1);
	int ret = filename.findRev("/");
	QString currentPath = filename.left(ret); // Strip off the filename, so we get just a directory
	if(ret == -1)
		currentPath = "/";
	
	
	lastFileName = filename;
	statusBar()->clear();
   
   listView->clear();
   listView->setRootIsDecorated(true);
   
   neededLibVector.clear();
	//we add items to the neededLibVector inside the process_file function
	int res = process_file(strcpy(tmp, filename.ascii()), this);   // we check if the given filename is a valid ELF 
	if(res == 0)
		statusLabel->setText("File " + filename + " loaded");
	else
		statusLabel->setText(" ");
		
	traverse(currentPath);

	
   //for each library on the 'first level'
   for(unsigned int i=0; i < neededLibVector.size(); i++)
   {  
		QString fullPath, dirname;
		QString soname(neededLibVector[i].ascii());
		fullPath = findFullPath (soname);
		dirname = fullPath.left(fullPath.findRev("/"));
		//string soname contains only the name of the lib. dirname contains the lib's path
		new booleanListViewItem(listView, soname, dirname);     
   }
   
   //the first root
   booleanListViewItem *myChild = dynamic_cast <booleanListViewItem*> (listView->firstChild()); 
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
   
   deletedVector.clear();   
   free(tmp);
	
	return;	
}




void autopackage_ldd_Sub::findChildSlot(QListViewItem* expandedRoot)
{
   char* constBuf = (char*) calloc(512, 1);      
   booleanListViewItem *myChildNext = 0; // needed in item removal in a pointers list
   booleanListViewItem *p;
   
   if(typeid(*(expandedRoot)) == typeid(booleanListViewItem))
   {   
      p = dynamic_cast <booleanListViewItem*> (expandedRoot);
      if(p->beenExpanded == true)
         return;
   
      if(p->beenExpanded == false)
         p->beenExpanded = true;
   
   }  
   
   neededLibVector.clear();
   //the first root
   booleanListViewItem *myChild = dynamic_cast <booleanListViewItem*> (expandedRoot->firstChild()); 
   
   if(myChild->text(0) == "this should not be here")
   {
      myChildNext = dynamic_cast <booleanListViewItem*> (myChild->nextSibling());
      delete myChild;
      myChild = myChildNext;
   }
   
   while(myChild) 
   {
      resolver(myChild);
      myChild = dynamic_cast <booleanListViewItem*> (myChild->nextSibling());
   }

   
   for(unsigned int i=0; i < deletedVector.size(); i++)   
   {  
      delete deletedVector[i];
   }
   
   deletedVector.clear();   
   free(constBuf);
	
	return;
}




void autopackage_ldd_Sub::deleteChildrenSlot(QListViewItem *collapsedRoot)
{
  /* QListViewItem *myChildNext = 0; 
   
   neededLibVector.clear();
   QListViewItem *myChild = collapsedRoot->firstChild();
   
   while(myChild) 
   {
      myChildNext = myChild->nextSibling();
      delete myChild;
      myChild = myChildNext;
   }
//   new QListViewItem(collapsedRoot, "this should not be here"); */
	return;
}


void autopackage_ldd_Sub::aboutMenuItemSlot()
{
	
	QMessageBox::about(this, "About visual-ldd", 
							"Visual Dependency Walker is a tool that will show you a graphical tree of\n\
all the shared libraries that a given ELF binary links to. It can be used\n\
to figure out why your program is linked against a certain library, and to check\n\
for version conflicts.\n\n\
Version 1.2\n\n\
Authors: Filippos Papadopoulos\n\tDavid Sansome\n\n\
(c) 2003-2005 by The Autopackage crew");
	
	return;
}


void autopackage_ldd_Sub::aboutQtMenuItemSlot()
{
	QMessageBox::aboutQt(this);
	return;
}

