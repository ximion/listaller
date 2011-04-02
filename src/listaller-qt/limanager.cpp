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

#include "limanager.h"

#include <QtCore>
#include <listaller.h>
#include <glib.h>

using namespace Listaller;

static QString charToStringFree(char *text)
{
  QString res;
  res.fromLatin1 (text);
  g_free (text);
  return res;
}

namespace Listaller {
  class AppManagerPriv : public QObject
  {
    Q_OBJECT
    
  public:
      AppManagerPriv();
      ~AppManagerPriv();
      
      bool sumode ();
      void setSumode (bool b);
      
  private:
      ListallerManager *mgr;
      ListallerSettings *settings;
  };

  /* Private Listaller AppManager */

  AppManagerPriv::AppManagerPriv ()
  {
    settings = listaller_settings_new (false);
    mgr = listaller_manager_new (settings);
  }

  bool AppManagerPriv::sumode()
  {
    return listaller_settings_get_sumode (settings);
  }

  void AppManagerPriv::setSumode(bool b)
  {
    listaller_settings_set_sumode (settings, b);
  }
}

/* AppManager Class */

AppManager::AppManager()
{
  priv = new AppManagerPriv ();
}

AppManager::~AppManager()
{  
  delete priv;
}

bool AppManager::sumode()
{
  return priv->sumode ();
}

void AppManager::setSumode(bool b)
{
  priv->setSumode (b);
}
