/* downloader.vala - Downloads files from various sources on the internet
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
using Listaller;
using Listaller.Utils;

namespace Listaller {

private class Downloader : Object {
	private int bytes_received;
	public signal void progress (int step);

	public Downloader () {
		bytes_received = 0;
	}

	private void soup_got_chunk_cb (Soup.Message msg, Soup.Buffer chunk) {
		string? clen;
		int total;
		int current;

		clen = msg.response_headers.get_one ("Content-length");
		if (clen == null)
			total = 0;
		else
			total = clen.to_int ();

		bytes_received += (int) chunk.length;

		int prog = (int) Math.round (100 / total * bytes_received);
		progress (prog);
	}

	public bool download_file_sync (string remote_url_http, string local_name, bool pedantic = false) throws Error {
		var session = new Soup.SessionSync ();
		var message = new Soup.Message ("GET", remote_url_http);

		message.got_chunk.connect (soup_got_chunk_cb);

		bytes_received = 0;
		var status = session.send_message (message);
		if (status == 200) {
			FileUtils.set_contents (local_name,
	                                (string)message.response_body.data,
	                                (long)message.response_body.length);
			return true;
		} else {
			/*if (pedantic) {
				throw new IOError.INVALID_DATA ("Couldn't download file!\n%s".printf (e_query.message));
			} else {
				warning ("Cannot query file size, continuing with an unknown size.");
			}*/
			throw new IOError.FAILED ("Could not download file: %s\nStatus code: %u".printf (remote_url_http, status));
		}

		return false;
	}
}

} // End of namespace: Listaller
