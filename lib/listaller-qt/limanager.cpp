/*
    listaller-qt - Qt4 wrapper for libListaller
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

#include "limanager.h"

#include <QtCore>
#include <listaller>

using namespace Listaller;

#ifndef _LIMSGREDIRECT
#define _LIMSGREDIRECT

namespace Listaller {

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
    app.summary = ai->Summary;
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
#endif // _LIMSGREDIRECT

/* Listaller Callbacks */

void li_manager_status_change_cb(LI_STATUS status, LiStatusData data, LiMsgRedirect *rd)
{
  rd->sendStatusMessage(QString(data.text));
}

void li_manager_new_app_cb(char *name,LiAppInfo *obj, LiMsgRedirect *rd)
{
  rd->sendNewApp(obj);
}

LI_REQUEST_RES li_manager_message_cb(LI_MESSAGE mtype, const char *text, void* user_data)
{
  //Say yes to everything, until we have a nice request handler
  return LIRQS_Yes;
}

/* AppManager Class */
AppManager::AppManager()
{
  mgr = li_mgr_new();
  
  msgRedir = new LiMsgRedirect();
  connect(msgRedir, SIGNAL(statusMessage(QString)), this, SIGNAL(statusMessage(QString)));
  connect(msgRedir, SIGNAL(newApp(Application)), this, SIGNAL(newApp(Application)));
  
  //Catch status messages
  li_mgr_register_status_call(&mgr, LiStateEvent(li_manager_status_change_cb), msgRedir);
  //Catch new apps
  li_mgr_register_app_call(&mgr, LiNewAppEvent(li_manager_new_app_cb), msgRedir);
  
  li_mgr_register_message_call(&mgr, li_manager_message_cb, msgRedir);
  
  setSuMode(false);
}

AppManager::~AppManager()
{  
  li_mgr_free(&mgr);
  delete msgRedir;
}

void AppManager::findApps(const QString filter_text)
{
  li_mgr_find_app(&mgr, fAllApps, qPrintable(filter_text));
}

bool AppManager::updateAppDB()
{
  return li_mgr_update_appdb(&mgr);
}

void AppManager::setSuMode(bool b)
{
  li_mgr_set_sumode(&mgr, b);
}

bool AppManager::suMode() const
{
  return li_mgr_sumode(&mgr);
}

bool AppManager::uninstallApp(Application app)
{
  struct local {
     static char *qStringToChar(QString s)
     {
       return (char*) qPrintable(s);
     }
  };
  
  LiAppInfo ai;
  ai.RemoveId = local::qStringToChar(app.removeId);
  ai.Author = local::qStringToChar(app.author);
  ai.Dependencies = local::qStringToChar(app.dependencies);
  ai.Name = local::qStringToChar(app.name);
  //ai.PkType = local::qStringToChar(app.pkType);
  ai.Profile = local::qStringToChar(app.profile);
  //TODO: Convert every part of Application to AppInfo

  li_mgr_remove_app(&mgr, ai);
}
