/* doap.vala
 *
 * Copyright (C) 2011 Matthias Klumpp <matthias@tenstral.net>
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

namespace Listaller {

private class RDFQuery : Object {
	private RDF.Storage storage;
	private RDF.World world;
	private RDF.Parser parser;
	private RDF.Model model;
	private string dpath;

	static const string SPARQL = "sparql";

	public RDFQuery () {
		world = new RDF.World ();
		storage = new RDF.Storage (world, "hashes", "doap_q", "new='yes',hash-type='memory',dir='.'");
		parser = new RDF.Parser (world, "", "", null);
		model = new RDF.Model (world, storage, "");
	}

	public void add_location (string location) {
		if (location.has_prefix ("/"))
			dpath = "file://" + location;
		else
			dpath = location;

		debug ("Adding new RDF info from %s", dpath);

		var duri = new RDF.Uri (world, dpath);
		RDF.Stream stream = parser.parse_as_stream (duri, duri);
		model.add_statements (stream);
	}

	public RDF.QueryResults? query (string query_str, string query_language = "") {
		if (query_str == "")
			return null;
		if (query_language == "")
			query_language = SPARQL;

		var q = new RDF.Query (world, query_language, null, query_str, null);
		var qres = q.execute (model);

		return qres;
	}
}

private class DoapData : Object {
	private RDFQuery querier;
	private string path;


	public DoapData () {
		path = "";
		querier = new RDFQuery ();
	}

	public void add_file (string fname) {
		querier.add_location (fname);
		path = fname;
	}

	public AppItem? get_project () {
		string querystring = """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX doap: <http://usefulinc.com/ns/doap#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name, ?shortname, $description, ?shortdesc,
       ?homepage, ?bug, ?download, ?wiki, ?created
WHERE {
    ?project rdf:type doap:Project .
    ?project doap:homepage ?homepage .
    ?project doap:name ?name .
    OPTIONAL { ?project doap:shortname ?shortname }
    OPTIONAL { ?project doap:description $description }
    OPTIONAL { ?project doap:shortdesc ?shortdesc }
    OPTIONAL { ?project doap:created ?created }
    OPTIONAL { ?project doap:bug-database ?bug }
    OPTIONAL { ?project doap:download-page ?download }
    OPTIONAL { ?project doap:wiki ?wiki }
}
""";
		RDF.QueryResults? results = querier.query (querystring);
		RDF.Node? n = results.get_binding_value_by_name ("description");
		debug ("DOAP Project Desc Is: %s", n.get_literal_value_as_latin1 ());

		return null;
	}

}

} // End of namespace
