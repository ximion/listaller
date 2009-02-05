/***************************************************************************
 *   Copyright (C) 2009 by Matthias Klumpp   *
 *   matthias@matthias-linuxws   *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <dbus/dbus-glib.h>

int main(int argc, char *argv[])
{
  DBusGConnection *connection;
DBusGProxy *proxy;
GError *error = NULL;
gboolean ret;

connection = dbus_g_bus_get (DBUS_BUS_SESSION, NULL);
proxy = dbus_g_proxy_new_for_name (connection,
                             "org.freedesktop.PackageKit",
                             "/org/freedesktop/PackageKit",
                             "org.freedesktop.PackageKit");
/* execute sync method */
ret = dbus_g_proxy_call (proxy, "InstallPackageName", &error,
                         G_TYPE_STRING, "openoffice-clipart",
                         G_TYPE_INVALID, G_TYPE_INVALID);
if (!ret) {
        g_warning ("failed: %s", error->message);
        g_error_free (error);
}

  printf("Hello, world!\n");

  return EXIT_SUCCESS;
}



//gcc lipkit.c -I/usr/include/dbus-1.0 -I/usr/include/glib-2.0 -c 
