/*
    listaller-qt - Qt4 wrapper for Listaller
    Copyright (C) 2010-2011 Matthias Klumpp

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef LIAPPMANAGER_H
#define LIAPPMANAGER_H

#include<QtCore>

namespace Listaller {

struct AppItem
{
  QString name;
  QString id;
  QString summary;
  QString version;
  QString publisher;
  QString iconName;
  double  timeStamp;
  QString dependencies;
};

class AppManagerPriv;

/**
 * The AppManager class allows access to Listaller's application
 * management function.
 * This class provides all functions the li_mgr functions provide.
 *
 * @short Access to Listaller's app manager
 */
class AppManager : public QObject
{
  Q_OBJECT
  
public:
    AppManager();
    ~AppManager();
    
    bool sumode ();
    void setSumode (bool b);    
    
private:
    AppManagerPriv *priv;
};

};

#endif // LIMANAGER_H
