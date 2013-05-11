/* zfeed.vala
 *
 * Copyright (C) 2011-2013 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller.Dep {

private class Feed : Object {
	private string fname;
	private Xml.Doc* _xdoc;

	public string package_url { get; set; }
	public string impl_version { get; set; }

	internal Xml.Doc* xdoc {
		get { return _xdoc; }
		set { _xdoc = value; }
	}

	public Feed () {
		fname = "";
		xdoc = null;
		package_url = "";
		impl_version ="0";
	}

	~Feed () {
		if (xdoc != null)
			delete xdoc;
	}

	public bool open (string path) {
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
			critical (_("XML file is not a valid ZI feed!"));
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
					if (get_xproperty (iter, attr)->get_content () == attr_value) {
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

	protected string get_intf_info_str (string name) {
		if (name == "")
			return "";
		return get_xsubnode (interface_node (), name)->get_content ();
	}

	public void update_dependency_data (ref Dep.Module dep) {
		dep.full_name = get_intf_info_str ("name");
		dep.idname = get_intf_info_str ("name").replace (" ", "");
		dep.set_version (impl_version);
		dep.summary = get_intf_info_str ("summary");
		dep.description = get_intf_info_str ("description");
		dep.homepage = get_intf_info_str ("homepage");
	}

	public bool search_matching_dependency () {
		Xml.Node *impl = null;

		// Find implementation which matches the current system
		for (Xml.Node* iter = interface_node()->children; iter != null; iter = iter->next) {
			// Spaces between tags are also nodes, discard them
			if (iter->type != ElementType.ELEMENT_NODE) {
				continue;
			}
			if (iter->name == "implementation") {
				string arch = get_xproperty (iter, "arch")->get_content ().down ();
				if (PatternSpec.match_simple (arch, "*-i?86"))
					arch = "%s-%s".printf (arch.substring (0, arch.index_of ("-")), system_machine ());
				else
					arch = system_osname_arch ();
				if (get_xproperty (iter, "arch")->get_content ().down () == arch.down ()) {
					impl = iter;
					break;
				}
			}
		}
		if (impl == null)
			return false;

		// Set dependency version from dependency implementation
		impl_version = get_xproperty (impl, "version")->get_content ();
		package_url = get_xproperty (get_xsubnode (impl, "archive"), "href")->get_content ();
		if (package_url.strip () == "")
			return false;

		return true;
	}

}

} // End of Listaller.Dep namespace
