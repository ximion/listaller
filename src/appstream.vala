/* appstream.vala -- some helper functions to work with AppStream data
 *
 * Copyright (C) 2014 Matthias Klumpp <matthias@tenstral.net>
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
using Listaller;
using Appstream;

namespace Listaller {

/**
 * Converts an Appstream component to an AppItem.
 *
 * @raturns NULL if Component was of wrong type or another converion error occured.
 */
private AppItem? appstream_component_to_appitem (Appstream.Component cpt) {
	if (cpt.kind != ComponentKind.DESKTOP_APP)
		return null;

	// we will want to replace AppItem with AsComponent sooner or later
	var app = new AppItem.blank ();
	app.idname = cpt.pkgname;
	app.info = cpt;
	app.desktop_file = cpt.idname;
	app.set_license_name (cpt.project_license);

	// set version, after retting the latest release
	Release? release = null;
	GenericArray<Release> releases = cpt.get_releases ();
	uint64 timestamp = 0;
	for(uint i = 0; i < releases.length; i++) {
		Release r = releases.get (i);
		if (r.get_timestamp () > timestamp) {
			release = r;
			timestamp = r.get_timestamp ();
		}
	}
	if (release != null) {
		app.version = release.get_version ();
	}

	return app;
}

} // End of namespace: Listaller
