/* acutils.vala
 *
 * Copyright (C) 2011-2013 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU General Public License Version 3
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
 */

namespace Listaller.Extra {

	private string verify_install_target (string insttarget, string srcdir) {
		string ret = "";
		if (insttarget == "") {
			string isdir = IPK.find_ipk_source_dir (srcdir);
			if (isdir == null) {
				warning ("IPK source dir not found!");
				return "";
			} else {
				ret = Path.build_filename (isdir, "inst_target", null);
				if (!Path.is_absolute (ret))
					ret = Path.build_filename (Environment.get_current_dir (), ret, null);
			}
		} else {
			message (_("Using user-defined install target: %s"), insttarget);
			ret = insttarget;
		}
		return ret;
	}
} // End of namespace
