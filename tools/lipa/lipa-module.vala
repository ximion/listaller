/* lipa-module.vala -- Generic class for lipa modules
 *
 * Copyright (C) 2012-2014 Matthias Klumpp <matthias@tenstral.net>
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

using GLib;
using Listaller;

public enum LipaRole {
	NONE,
	INSTALLATION,
	REMOVAL;
}

public abstract class LipaModule : Object {
	protected bool use_shared_mode;

	public int error_code { get; protected set; }
	protected CmdProgressBar progress_bar;

	public LipaModule () {
		error_code = 0;
		use_shared_mode = is_root ();
		progress_bar = new CmdProgressBar ();
	}

	public abstract void terminate_action ();
}
