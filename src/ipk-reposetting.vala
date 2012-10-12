/* ipk-reposetting.vala - Classes defining special files used in Listaller's IPK repositories
 *
 * Copyright (C) 2012 Matthias Klumpp <matthias@tenstral.net>
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

using GLib;
using Gee;
using Listaller;
using Listaller.IPK;
using Listaller.Utils;

namespace Listaller.Repo {

/**
 * Settings of an IPK package repository
 */
private class Settings : Object {
	private MetaFile data;

	public Settings () {
		data = new MetaFile ();
	}

	public bool open (string fname) {
		return data.open_file (fname);
	}
}

/**
 * Access an IPK-repo content-index
 */
private class ContentIndex : Object {
	private MetaFile data;

	public ContentIndex () {
		data = new MetaFile ();
	}

	public bool open (string fname) {
		return data.open_file (fname);
	}
}

} // End of namespace: Listaller.Repo
