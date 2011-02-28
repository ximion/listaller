/* manager.vala
 *
 * Copyright (C) 2010-2011  Matthias Klumpp
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

using GLib;

public class LiManager : Object {
	private bool sumode;

	public bool superuserMode {
		get { return sumode; }
		set { sumode = value; }
	}

	public LiManager (bool root) {
		sumode = root;
		debug ("This is Listaller " + LISTALLER_VERSION);
	}

	public void test () {
		SoftwareDB sdb = new SoftwareDB (false);
		sdb.open ();
	}
}
