/* lbutils.vala
 *
 * Copyright (C) 2011  Matthias Klumpp
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
 *
 * Info: Utils for IPK builder tools
 */

namespace Listaller.IPK {

	private string? validate_srcdir (string dir) {
		// Check if IPK sources are present
		string tmp = dir;
		if (FileUtils.test (tmp, FileTest.IS_DIR)) {
			if (FileUtils.test (Path.build_filename (tmp, "control.xml", null), FileTest.EXISTS) &&
				FileUtils.test (Path.build_filename (tmp, "files-current.list", null), FileTest.EXISTS)) {
				// Set current source dir and exit
				return tmp;
				}
		}
		return null;
	}

	private string? find_ipk_source_dir (string origdir) {
		string tmp = validate_srcdir (Path.build_filename (origdir, "ipkinstall", null));
		if (tmp == null) {
			tmp = validate_srcdir (Path.build_filename (origdir, "install", null));
			if (tmp == null) {
				tmp = validate_srcdir (Path.build_filename (origdir, "data", "install", null));
			}
		}
		return tmp;
	}

} // End of namespace
