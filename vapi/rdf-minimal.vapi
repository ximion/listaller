/* rdf-minimal.vapi - Simple bindings for Redland RDF
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

[CCode (cprefix="librdf_", lower_case_cprefix="librdf_", cheader_filename = "librdf.h")]
namespace RDF {

	[Compact]
	[CCode (cname="librdf_storage", free_function="librdf_free_storage", cprefix="librdf_storage_")]
	public class Storage {
		[CCode (cname="librdf_new_storage")]
		public Storage (World world, string storage_name, string name, string options_str);
	}

	[Compact]
	[CCode (cname="librdf_world", free_function="librdf_free_world", cprefix="librdf_world_")]
	public class World {
		[CCode (cname="librdf_new_world")]
		public World ();
		public void open ();
		public void init_mutex ();
	}

	[Compact]
	[CCode (cname="librdf_model", free_function="librdf_free_model", cprefix="librdf_model_")]
	public class Model {
		[CCode (cname="librdf_new_model")]
		public Model (World world, Storage storage, string options_str);
		public int add_statement (Statement stmt);
		public int add_statements (Stream statement_stream);
		public int size ();

	}

	[Compact]
	[CCode (cname="librdf_parser", free_function="librdf_free_parser", cprefix="librdf_parser_")]
	public class Parser {
		[CCode (cname="librdf_new_parser")]
		public Parser (World world, string? name, string? mime_type, Uri? type_uri);
		public Stream parse_as_stream (Uri uri, Uri base_uri);
		public Stream parse_string_as_stream ([CCode(type="const unsigned char *")] string str, Uri? base_uri);

	}

	[Compact]
	[CCode (cname="librdf_uri", free_function="librdf_free_uri", cprefix="librdf_uri_")]
	public class Uri {
		[CCode (cname="librdf_new_uri")]
		public Uri (World world, string uri_string);
		public string to_string ();

	}

	[Compact]
	[CCode (cname="librdf_stream", free_function="librdf_free_stream", cprefix="librdf_stream_")]
	public class Stream {
		/* [CCode (cname="librdf_new_stream")]
		public Stream (World world, void* context, int (*is_end_method)(void*), int (*next_method)(void*), void* (*get_method)(void*, int), void (*finished_method)(void*));
		*/
	}

	[Compact]
	[CCode (cname="librdf_statement", free_function="librdf_free_statement", cprefix="librdf_statement_")]
	public class Statement {

	}

	[Compact]
	[CCode (cname="librdf_query", free_function="librdf_free_query", cprefix="librdf_query_")]
	public class Query {
		[CCode (cname="librdf_new_query")]
		public Query (World world, string name, Uri? uri, string query_string, Uri base_uri);
		public QueryResults execute(Model model);

	}

	[Compact]
	[CCode (cname="librdf_query_results", free_function="librdf_free_query_results", cprefix="librdf_query_results_")]
	public class QueryResults {
		public int get_count ();
		public bool next ();
		public bool finished ();
		public Node get_binding_value (int offset);
		public Node get_binding_name (int offset);
		public Node get_binding_value_by_name (string name);

	}

	[Compact]
	[CCode (cname="librdf_node", free_function="librdf_free_node", cprefix="librdf_node_")]
	public class Node {
		[CCode (cname="librdf_new_node")]
		public Node (World world);
		public string to_string ();
		public string get_literal_value_as_latin1 ();
		public Uri get_literal_value_datatype_uri ();
		public string get_literal_value_language ();
		public bool is_literal ();
		public bool is_blank ();
		public bool is_resource ();
	}

}
