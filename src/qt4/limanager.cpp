/* listaller-qt - Qt4 wrapper for Listaller
 * Copyright (C) 2010-2011 Matthias Klumpp <matthias@nlinux.org>
 *
 * Licensed under the GNU Lesser General Public License Version 3
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "limanager.h"

#include <QtCore>
#include <listaller.h>
#include <glib.h>

using namespace Listaller;

static QString charToStringFree (char *text)
{
  QString res;
  res.fromLatin1 (text);
  g_free (text);
  return res;
}

AppManager::AppManager ()
{
  conf = listaller_settings_new (false);
  mgr = listaller_manager_new (conf);
}

AppManager::~AppManager ()
{
  g_object_unref (mgr);
  g_object_unref (conf);
}

bool AppManager::sumode ()
{
  return listaller_settings_get_sumode (conf);
}

void AppManager::setSumode (bool b)
{
  listaller_settings_set_sumode (conf, b);
}
