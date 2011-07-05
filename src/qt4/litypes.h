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

#ifndef LITYPES_H
#define LITYPES_H

// Dummy Listaller classes
typedef struct _ListallerSettings ListallerSettings;
typedef struct _ListallerManager ListallerManager;
typedef struct _ListallerSetup ListallerSetup;
typedef struct _ListallerIPKControl ListallerIPKControl;

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

}

#endif // LITYPES_H
