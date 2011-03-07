/* ipkcdef.vala
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
 */

using GLib;
using Xml;

// Workaround for Vala bug #618931
private const string _PKG_VERSION5 = Config.VERSION;

private class IPKCXml : Object {
	private int indent = 0;
	private string fname;
	private Xml.Doc* xdoc;

	public IPKCXml () {
		fname = "";
		xdoc = null;
	}

	~IPKCXml () {
		if (xdoc != null)
			delete xdoc;
	}

	public bool open (string path) {
		// Already opened?
		if (xdoc != null) {
			warning ("You have to close the IPK XML first to reopen a new one!");
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
		if ((root == null) || (root->name != "ipkcontrol")) {
			warning (_("XML file '%s' is damaged."), path);
			return false;
		}

		// If we got here, everything is fine
		return true;
	}

	public bool create_new () {
		// Already opened?
		if (xdoc != null) {
			warning ("You have to close the IPK XML first to create a new one!");
			return false;
		}

		// Helber node
		Xml.Node* nd;

		xdoc = new Doc ("1.0");
		Xml.Node* root = new Xml.Node (null, "ipkcontrol");
		xdoc->set_root_element (root);

		root->new_prop ("version", "1.1");

		Xml.Node* subnode = root->new_text_child (null, "application", "");
		nd = subnode->new_text_child (null, "id", "test.desktop" );
		nd->new_prop ("type", "desktop");

		Xml.Node* comment = new Xml.Node.comment ("To be continued");
		root->add_child (comment);
		return true;
	}

	public void print_xml () {
		string xmlstr;
		// This throws a compiler warning, see bug 547364
		xdoc->dump_memory (out xmlstr);

		debug (xmlstr);
	}

	private Xml.Node* root_node () {
		Xml.Node* root = xdoc->get_root_element ();
		if ((root == null) || (root->name != "ipkcontrol")) {
			critical (_("XML file is damaged or XML structure is no valid IPKControl!"));
			return null;
		}
		return root;
	}

	private Xml.Node* app_node () {
		Xml.Node* appnd = null;
		for (Xml.Node* iter = root_node ()->children; iter != null; iter = iter->next) {
			// Spaces between tags are also nodes, discard them
			if (iter->type != ElementType.ELEMENT_NODE) {
				continue;
			}
			if (iter->name == "application") {
				appnd = iter;
				break;
			}
		}
		if (appnd == null)
			critical (_("XML file is damaged or XML structure is no valid IPKControl!"));
		return appnd;
	}

	private Xml.Node* get_appdata_node (string id) {
		Xml.Node* res = null;
		Xml.Node* an = app_node ();
		assert (an != null);

		for (Xml.Node* iter = an->children; iter != null; iter = iter->next) {
			// Spaces between tags are also nodes, discard them
			if (iter->type != ElementType.ELEMENT_NODE) {
				continue;
			}
			if (iter->name == id) {
				res = iter->get_content ();
				break;
			}
		}
		// If node was not found, create new one
		if (res == null)
			res = an->new_text_child (null, id, "");
		return res;
	}

	private string get_node_content (Xml.Node* nd) {
		string ret = "";
		ret = nd->get_content ();
		// TODO: Translate the string
		return ret;
	}

	// Setter/Getter methods for XML properties
	public void set_app_name (string s) {
		get_appdata_node ("name")->set_content (s);
	}

	public string get_app_name () {
		return get_node_content (get_appdata_node ("name"));
	}

}
