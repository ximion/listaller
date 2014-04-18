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

namespace Listaller {

/**
 * Converts an Appstream component to an AppItem.
 *
 * @raturns NULL if Component was of wrong type or another converion error occured.
 */
private AppItem? appstream_component_to_appitem (Appstream.Component cpt) {
	if (cpt.kind != Appstream.ComponentKind.DESKTOP_APP)
		return null;

	// we will want to replace AppItem with AsComponent sooner or later
	var app = new AppItem.blank ();
	app.idname = cpt.idname;
	app.full_name = cpt.name;
	app.summary = cpt.summary;
	app.description = cpt.description;
	app.desktop_file = cpt.idname;
	app.website = cpt.homepage;
	app.icon_name = cpt.icon;

	return app;
}

} // End of namespace: Listaller
