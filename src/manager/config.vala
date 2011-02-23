/* config.vala
 *
 * Copyright (C) 2010  Matthias Klumpp
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Author:
 * 	Matthias Klumpp <matthias@nlinux.org>
 */

using PkgConfig;
using GLib;

// Required to include config.h as _first_ unit
private const string inv_4 = PkgConfig.VERSION;

private class LiConfig : Object {
	private bool sumode;
	const string suconfdir = "/etc/lipa";

	public bool superuserMode {
		get { return sumode; }
		set { sumode = value; }
	}

	public LiConfig (bool root) {
		sumode = root;
	}

	public string database_file () {
		if (sumode) {
			return suconfdir + "/software.db";
		} else {
			return Environment.get_user_config_dir () + "software/software.db";
		}
	}

	public string appregister_dir () {
		if (sumode) {
			return suconfdir;
		} else {
			return Environment.get_user_config_dir () + "software";
		}
	}
}

int main(string[] args)
{
	// Initialize localisation
	Intl.bindtextdomain (PkgConfig.GETTEXT_PACKAGE, PkgConfig.LOCALEDIR);
	Intl.bind_textdomain_codeset (PkgConfig.GETTEXT_PACKAGE, "UTF-8");
	Intl.textdomain (PkgConfig.GETTEXT_PACKAGE);
	return 0;
}
