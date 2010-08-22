/*
    libinstaller-qt - Qt4 wrapper for libinstaller
    Copyright (C) 2010 Matthias Klumpp

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "limanager.h"

#include<QtGlobal>
#include<glib.h>

LiAppManager::LiAppManager()
{
  mgr = li_mgr_new();
}

LiAppManager::~LiAppManager()
{
  li_mgr_free(mgr);
}

bool LiAppManager::loadApps()
{
  return li_mgr_load_apps(mgr);
}
