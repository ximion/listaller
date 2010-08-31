/*
    libinstaller-qt - Qt4 wrapper for libListaller
    Copyright (C) 2010 Matthias Klumpp

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

#include<QtGui>

namespace Listaller {

class LiMsgRedirect;

struct AppData
{
  QString name;
  QString pkName;
  QString shortDesc;
  QString version;
  QString author;
  QString iconName;
  QString profile;
  QString uId;
  double installDate;
  QString dependencies;
};

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
    
    bool loadApps();
    
    void setSuMode(bool b);
    
signals:
    void newApp(AppData app);
    void statusMessage(QString msg);

private slots:
    void emitStatusMessage(QString s) {emit(statusMessage(s));};
    void emitNewApp(AppData app) {emit(newApp(app));};
    
private:
    void* mgr;
    LiMsgRedirect *msgRedir;
};

};

#endif // LIMANAGER_H
