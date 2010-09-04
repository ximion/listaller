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

#ifndef LIMSGREDIRECT_H
#define LIMSGREDIRECT_H

#include <QtCore>
#include <Listaller>
#include "limanager.h"

namespace Listaller {

/**
 * This class forwards messages from Listaller callbacks to their
 * destination Qt classes.
 *
 * @short Message redirector class
 */
class LiMsgRedirect : public QObject
{
  Q_OBJECT
  
public:
  void sendStatusMessage(QString s){ emit(statusMessage(s)); }
  void sendNewApp(LiAppInfo *ai){
    Listaller::Application app;
    app.author = ai->Author;
    app.name = ai->Name;
    app.pkName = ai->PkName;
    app.shortDesc = ai->ShortDesc;
    app.version = ai->Version;
    app.installDate = ai->InstallDate;
    app.iconName = ai->IconName;
    emit(newApp(app));  
  }
  
signals:
  void statusMessage(QString s);
  void newApp(Application app);
  
};

};

#endif // LIMSGREDIRECT_H
