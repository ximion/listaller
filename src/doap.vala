/* doap.vala
 *
 * Copyright (C) 2011-2012 Matthias Klumpp <matthias@tenstral.net>
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

protected errordomain RdfError {
	NO_RESULTS,
	INVALID_PATH,
	INVALID_QUERY;
}

private class RDFQuery : Object {
	private RDF.Storage storage;
	private RDF.World world;
	private RDF.Parser *parser;
	private RDF.Model model;
	private string dpath;

	static const string SPARQL = "sparql";

	public RDFQuery (string? parserName = null) {
		world = new RDF.World ();
		storage = new RDF.Storage (world, "hashes", "doap_q", "new='yes',hash-type='memory',dir='.'");
		parser = new RDF.Parser (world, parserName, null, null);
		model = new RDF.Model (world, storage, "");
	}

	~RDFQuery () {
		// We handle the parser manually to work around a bug with newer libRDF versions
		if (parser != null)
			delete parser;
	}

	public void add_location (string location) {
		if (location.has_prefix ("/"))
			dpath = "file://" + location;
		else
			dpath = location;

		debug ("Adding new RDF info from path %s", dpath);

		var duri = new RDF.Uri (world, dpath);
		RDF.Stream stream = parser->parse_as_stream (duri, duri);
		model.add_statements (stream);
	}

	public void add_location_str (string data, string baseUrl) {
		debug ("Adding new RDF info from string");

		RDF.Stream stream = parser->parse_string_as_stream (data, new RDF.Uri (world, baseUrl));
		model.add_statements (stream);
	}

	public RDF.QueryResults? query (string query_str, string query_language = "") throws RdfError {
		if (query_str == "")
			throw new RdfError.INVALID_QUERY ("Query string was empty!");
		if (query_language == "")
			query_language = SPARQL;

		var q = new RDF.Query (world, query_language, null, query_str, null);
		var qres = q.execute (model);

		return qres;
	}
}

private const string doapQueryHead = """
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX doap: <http://usefulinc.com/ns/doap#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
""";

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

	public void add_data (string data) {
		querier.add_location_str (data, "http://usefulinc.com/ns/doap#");
	}

	public string get_doap_url () {
		return path;
	}

	private string node_str_value_by_name (RDF.QueryResults results, string name) {
		RDF.Node? n = results.get_binding_value_by_name (name);

		if (n == null)
			return "";

		RDF.Uri? uri = n.get_literal_value_datatype_uri ();
		string suri = "NULL";
		if (uri != null)
			suri = uri.as_string ();
		//! debug ("Node datatype ('%s'): %s", name, suri);

		if (n.is_resource ()) {
			RDF.Uri? vUri = new RDF.Uri.from_uri (n.get_uri ());
			string? s = vUri.as_string ();
			if (s == null)
				s = "";
			return s;
		}

		if (!n.is_literal ())
			return "";

		return n.get_literal_value_as_latin1 ();
	}

	private string get_newest_release_version () throws RdfError {
		string querystring = doapQueryHead + """
SELECT ?name, ?revision, ?created, ?description
WHERE {
      ?project rdf:type doap:Project .
      ?project doap:release ?release .
      ?release doap:name ?name .
      ?release doap:revision ?revision .
      ?release doap:created ?created
      OPTIONAL { ?release dc:description ?description }
}
ORDER BY DESC(?created)
""";
		RDF.QueryResults? qres = querier.query (querystring);
		if (qres == null)
			throw new RdfError.NO_RESULTS (_("Query returned no results. Maybe there's no release defined?"));

		string version = "";
		if (qres.is_bindings ()) {
			do {
				version = node_str_value_by_name (qres, "revision");

				// This code can be used to fetch more information about this version
				/* for (int i = 0; i < qres.get_bindings_count (); i++) {
					if (qres.get_binding_name (i) == "revision") {
						var n = qres.get_binding_value (i);
					if (n != null)
						version = n.get_literal_value_as_latin1 ();
					}

				} */

				qres.next ();
			} while ((!qres.finished ()) && (version == ""));
		}

		return version;
	}

	public AppItem? get_project () throws RdfError {
		string querystring = doapQueryHead + """
SELECT ?name, ?shortname, $description, ?shortdesc,
       ?homepage, ?bug, ?download, ?wiki, ?created, ?license
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
    OPTIONAL { ?project doap:license ?license }
}
""";
		RDF.QueryResults? qres = querier.query (querystring);
		if (qres == null)
			throw new RdfError.NO_RESULTS (_("Query returned no results. Maybe this DOAP data is broken?"));

		// Create a new AppItem to store project metadata
		AppItem app = new AppItem.blank ();

		app.full_name = node_str_value_by_name (qres, "name");
		app.idname = node_str_value_by_name (qres, "shortname");
		app.description = node_str_value_by_name (qres, "description");
		app.summary = node_str_value_by_name (qres, "shortdesc");
		app.website = node_str_value_by_name (qres, "homepage");
		app.set_license_from_doap_name (node_str_value_by_name (qres, "license"));

		try {
			app.version = get_newest_release_version ();
		} catch (Error e) {
			li_error ("Could not fetch version from DOAP! (%s)".printf (e.message));
		}

		// TODO: Fetch all useful data from doap file

		return app;
	}

}

} // End of namespace
