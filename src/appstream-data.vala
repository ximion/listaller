/* appstream-data.vala
 *
 * Copyright (C) 2012-2013 Matthias Klumpp <matthias@tenstral.net>
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
using Xml;
using Listaller;

namespace Listaller {

private errordomain ASParserError {
		UNKNOWN,
		TAG_NOT_FOUND;
}

/**
 * Implements a superset of the AppStream XML spec for Listaller to use.
 * It adds the following elements to the original AppStream spec:
 *  - version
 *  - author
 */
private class AppStreamXML : Object {
	private Xml.Doc* doc;
	private string locale;

	public AppStreamXML () {
		doc = null;
		locale = Intl.get_language_names ()[0];
	}

	~AppStreamXML () {
		delete doc;
	}

	private string? parse_value (Xml.Node *node, bool translated = false) {
		string content = node->get_content ();
		string? lang = node->get_prop ("lang");
		if (translated) {
			// FIXME: If not-localized generic node comes _after_ the localized ones,
			//        the not-localized will override the localized. Wrong ordering should
			//        not happen. (but this code can be improved anyway :P)
			if (lang == null)
				return content;
			if (lang == locale)
				return content;
			if (lang == locale.split("_")[0])
				return node->get_content ();

			// Haven't found a matching locale
			return null;
		}
		// If we have locale here, but want the untranslated item
		if (lang != null)
			return null;

		return content;
	}

	private string[] get_childs_as_array (Xml.Node* node, string element_name) {
		string[] list = {};
		for (Xml.Node* iter = node->children; iter != null; iter = iter->next) {
			// Discard spaces
			if (iter->type != ElementType.ELEMENT_NODE) {
				continue;
			}

			if (iter->name == element_name) {
				string? content = iter->get_content ();
				if (content != null)
					list += content.strip ();
			}
		}

		return list;
	}

	public bool load_file (string fname) {
		bool ret = true;

		if (doc != null)
			delete doc;

		// Parse the document from path
		doc = Parser.parse_file (fname);
		if (doc == null) {
			stderr.printf ("File %s not found or permissions missing", fname);
			return false;
		}

		Xml.Node* root = doc->get_root_element ();
		if (root == null) {
			delete doc;
			stderr.printf ("The XML file '%s' is empty", fname);
			return false;
		}

		return ret;
	}

	public bool load_data (string data) {
		bool ret = true;

		if (doc != null)
			delete doc;

		// Parse the document from path
		doc = Parser.parse_doc (data);
		if (doc == null) {
			stderr.printf ("AppStream XML data could not be loaded.");
			return false;
		}

		Xml.Node* root = doc->get_root_element ();
		if (root == null) {
			delete doc;
			stderr.printf ("The XML data is empty");
			return false;
		}

		return ret;
	}

	private AppItem? parse_application_node (Xml.Node* node) throws ASParserError {
		var app = new AppItem.blank ();
		for (Xml.Node* iter = node->children; iter != null; iter = iter->next) {
			if (iter->type != ElementType.ELEMENT_NODE) {
				continue;
			}

			string node_name = iter->name;
			string? content = parse_value (iter);
			switch (node_name) {
				case "id":	if (content != null) {
							// in this case, ID == desktop-file
							app.desktop_file = content;
						}
						break;
				case "pkgname": if (content != null)
							app.idname = content;
						break;
				case "name": 	if (content != null) {
							app.full_name = content;
						} else {
							content = parse_value (iter, true);
							if (content != null)
								app.full_name = content;
						}
						break;
				case "summary": if (content != null) {
							app.summary = content;
						} else {
							content = parse_value (iter, true);
							if (content != null)
								app.summary = content;
						}
						break;
				case "icon":	if (node->get_prop ("type") == "stock")
							if (content != null)
								app.icon_name = content;
						break;
				case "url":	if (content != null)
							app.website = content;
						break;
				case "version":	if (content != null)
							app.version = content;
						break;
				case "project_license":	if (content != null)
							app.set_license_name (content);
						break;
				case "categories":
						string[] cat_array = get_childs_as_array (iter, "category");
						string cat_str = "";
						foreach (string s in cat_array)
							cat_str = "%s%s;".printf (cat_str, s);
						app.categories = cat_str;
						break;
			}
		}

		if (Utils.str_is_empty (app.version))
			throw new ASParserError.TAG_NOT_FOUND (_("AppData does not define a version number for this application!"));


		// sanity checks
		app.fast_check ();

		return app;
	}

	public AppItem? get_app_item () throws ASParserError {
		if (doc == null) {
			warning ("No AppStream data is set. Could not return AppItem.");
			return null;
		}
		AppItem? item = null;
		Xml.Node* root = doc->get_root_element ();
		if (root->name == "application") {
			try {
				item = parse_application_node (root);
			} catch (ASParserError e) {
				throw e;
			}
		} else {
			for (Xml.Node* iter = root->children; iter != null; iter = iter->next) {
				// Discard spaces
				if (iter->type != ElementType.ELEMENT_NODE) {
					continue;
				}

				if (iter->name == "application") {
					try {
						item = parse_application_node (iter);
					} catch (ASParserError e) {
						throw e;
					}
				}
			}
		}

		return item;
	}

}

} // End of namespace: Listaller
