/* -*- Mode: C; tab-width: 8; indent-tabs-mode: t; c-basic-offset: 8 -*-
 *
 * listaller-qt - Qt4 wrapper for Listaller
 * Copyright (C) 2010-2011 Matthias Klumpp <matthias@nlinux.org>
 *
 * Licensed under the GNU Lesser General Public License Version 3+
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

#include "lisetup.h"

#include<QtCore>
#include<listaller.h>

using namespace Listaller;

Setup::Setup (QString pkgFileName)
{
  conf = listaller_settings_new (false);
  setup = listaller_setup_new (qPrintable(pkgFileName), conf);
}

Setup::~Setup ()
{
  g_object_unref (setup);
  g_object_unref (conf);
}

bool Setup::initialize ()
{
  bool ret = false;
  ret = listaller_setup_initialize (setup);
  ipkmeta = listaller_setup_get_control (setup);
  if (ipkmeta == NULL)
    ret = false;

  return ret;
}

void Setup::setSuMode (bool b)
{
  listaller_settings_set_sumode (conf, b);
}

bool Setup::suMode () const
{
  return listaller_settings_get_sumode (conf);
}

QString Setup::descriptionAsString () const
{
  gchar *text = listaller_ipk_cxml_get_app_description (&ipkmeta->parent_instance);
  QString res;
  res.fromLatin1 (text);
  g_free (text);
  return res;
}

void Setup::setTestmode (bool b)
{
  listaller_settings_set_testmode (conf, b);
}

bool Setup::testmode () const
{
  return listaller_settings_get_testmode (conf);
}

bool Setup::runInstallation ()
{
  return listaller_setup_run_installation (setup);
}
