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

		string xmlstr;
		// This throws a compiler warning, see bug 547364
		xdoc->dump_memory (out xmlstr);

		debug (xmlstr);
		return true;
	}

}
