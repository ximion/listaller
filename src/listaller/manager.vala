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
	private LiSettings conf;

	public signal void error_code (LiErrorItem error);
	public signal void progress_changed (int progress, int subprogress);
	public signal void status_changed (LiStatusItem status);

	public LiSettings settings {
		get { return conf; }
		set { conf = value; }
	}

	/*
	 * @param: settings A valid LiSettings instance, describing basic settings (or null)
	 */
	public LiManager (LiSettings? settings) {
		conf = settings;
		if (conf == null)
			conf = new LiSettings (false);

		debug ("This is Listaller " + Config.VERSION);
	}

	public void test () {
		SoftwareDB sdb = new SoftwareDB (conf);
		sdb.open ();
	}
}
