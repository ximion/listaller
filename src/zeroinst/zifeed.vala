/* zifeed.vala
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@nlinux.org>
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
using Gee;
using Listaller;
using Listaller.Utils;

namespace Listaller.ZeroInstall {

private abstract class Feed : Object {
	private string fname;
	private Xml.Doc* _xdoc;

	internal Xml.Doc* xdoc {
		get { return _xdoc; }
		set { _xdoc = value; }
	}

	internal Feed () {
		fname = "";
		xdoc = null;
	}

	~Feed () {
		if (xdoc != null)
			delete xdoc;
	}

	protected bool open (string path) {
		// Already opened?
		if (xdoc != null) {
			warning ("This ZI feed has already been opened!");
			return false;
		}
		fname = path;

		// Parse the document from path
		xdoc = Parser.parse_file (fname);
		if (xdoc == null) {
			warning (_("File %s not found or permission denied!"), path);
			return false;
		}

		// Get the root node
		Xml.Node* root = xdoc->get_root_element ();
		if ((root == null) || (root->name != "interface")) {
			warning (_("ZI XML file '%s' is damaged."), path);
			return false;
		}

		// If we got here, everything is fine
		return true;
	}

	private Xml.Node* interface_node () {
		Xml.Node* root = xdoc->get_root_element ();
		if ((root == null) || (root->name != "interface")) {
			critical (_("XML file is no valid ZI feed!"));
			return null;
		}
		return root;
	}

	internal Xml.Node* get_xsubnode (Xml.Node* sn, string id, string attr = "", string attr_value = "") {
		Xml.Node* res = null;
		assert (sn != null);

		for (Xml.Node* iter = sn->children; iter != null; iter = iter->next) {
			// Spaces between tags are also nodes, discard them
			if (iter->type != ElementType.ELEMENT_NODE) {
				continue;
			}
			if (iter->name == id) {
				if (attr != "") {
					if (get_node_content (get_xproperty (iter, attr)) == attr_value) {
						res = iter;
						break;
					}
				} else {
					res = iter;
					break;
				}
			}
		}
		// If node was not found, create new one
		if (res == null) {
			res = sn->new_text_child (null, id, "");
			if (attr != "")
				res->new_prop (attr, attr_value);
		}
		return res;
	}

	internal Xml.Node* get_xproperty (Xml.Node* nd, string id) {
		Xml.Node* res = null;
		assert (nd != null);
		for (Xml.Attr* prop = nd->properties; prop != null; prop = prop->next) {
			string attr_name = prop->name;
			if (attr_name == id) {
				res = prop->children;
				break;
			}
		}
		// If no property was found, create new one
		if (res == null)
			res = nd->new_prop (id, "")->children;
		return res;
	}

}

} // End of Listaller.ZeroInstall namespace
