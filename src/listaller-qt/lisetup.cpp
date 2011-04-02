/*
    listaller-qt - Qt4 wrapper for Listaller
    Copyright (C) 2010-2011 Matthias Klumpp

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

#include "lisetup.h"

#include<QtCore>
#include<listaller>

using namespace Listaller;

Setup::Setup()
{
  setup = li_setup_new ();
}

Setup::~Setup()
{
  li_object_free (setup);
}

void Setup::initialize(QString pkgName)
{
  li_setup_init (setup, (char*) qPrintable(pkgName));
}

void Setup::setSuMode(bool b)
{
  li_setup_set_sumode (setup, b);
}

bool Setup::suMode() const
{
  return li_setup_sumode (setup);
}

QString Setup::disallows() const
{
  return li_setup_disallows (setup);
}

QString Setup::supportedDistributions() const
{
  return li_setup_supported_distros (setup);
}

QString Setup::appName() const
{
  LiAppItem *ai;
  ai = li_setup_appitem (setup);
  QString aname = li_appitem_name(ai);
  li_object_free(ai);
  return aname;
}

QString Setup::appVersion() const
{
  LiAppItem *ai;
  ai = li_setup_appitem (setup);
  QString version = li_appitem_version(ai);
  li_object_free(ai);
  return version;
}

QString Setup::descriptionAsString() const
{
  return QString(li_setup_long_description_as_string(setup));
}

void Setup::setTestmode(bool b)
{
  li_setup_set_testmode(setup, b);
}
