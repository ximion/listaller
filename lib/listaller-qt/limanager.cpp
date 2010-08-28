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

#include "limanager.h"

#include<QtCore>
#include<listaller>
#include "limsgredirect.h"

using namespace Listaller;


/* Listaller Callbacks */
void manager_status_change_cb(LiStatusChange change, TLiStatusData data, LiMsgRedirect *msg)
{
  msg->sendStatusMessage(QString(data.msg));
}

void manager_new_app_cb(char *name,AppInfo obj,LiMsgRedirect *msg)
{
  msg->sendNewApp(obj);
}

/* AppManager Class */
AppManager::AppManager()
{
  mgr = li_mgr_new();
  
  msgRedir = new LiMsgRedirect();
  connect(msgRedir, SIGNAL(statusMessage(QString)), this, SLOT(emitStatusMessage(QString)));
  connect(msgRedir, SIGNAL(newApp(AppData)), this, SLOT(emitNewApp(AppData)));
  
  //Catch status messages
  li_mgr_register_status_call(mgr, StatusChangeEvent(manager_status_change_cb), msgRedir);
  //Catch new apps
  li_mgr_register_app_call(mgr, NewAppEvent(manager_new_app_cb), msgRedir);
  
  li_mgr_set_sumode(mgr, false);
}

AppManager::~AppManager()
{
  li_mgr_free(mgr);
  delete msgRedir;
}

bool AppManager::loadApps()
{
  return li_mgr_load_apps(mgr);
}

void AppManager::setSuMode(bool b)
{
  li_mgr_set_sumode(mgr, b);
}
