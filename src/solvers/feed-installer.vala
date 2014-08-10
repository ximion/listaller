/* feed-installer.vala
 *
 * Copyright (C) 2011-2014 Matthias Klumpp <matthias@tenstral.net>
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
using Gee;
using Listaller;
using Listaller.Utils;

namespace Listaller.Dep {

private class FeedInstaller : MessageObject {
	private Feed? feed;
	private SetupSettings ssettings;
	private string tmpdir;

	public ErrorItem? last_error { get; set; }

	public FeedInstaller (SetupSettings setup_settings) {
		last_error = null;
		feed = null;

		ssettings = setup_settings;
		var conf = new Config ();
		tmpdir = conf.get_unique_tmp_dir ("feedwork");
	}

	~FeedInstaller () {
		// Remove the temporary dir
		delete_dir_recursive (tmpdir);
	}

	private new void emit_error (ErrorEnum id, string details) { }

	private void set_error (ErrorEnum id, string details) {
		// Construct error
		ErrorItem item = new ErrorItem (id);
		item.details = details;
		last_error = item;
		debug ("FeedInstaller: %s", details);
	}

	private async void do_download (File remote, File local, MainLoop? loop, FileProgressCallback? on_progress) {
		try {
			try {
				yield remote.find_enclosing_mount_async (0);
			} catch (IOError e_mount) {
				// Not mounted...
			}

			int64 size = 0;
			try {
				FileInfo info = yield remote.query_info_async (FileAttribute.STANDARD_SIZE,
					FileQueryInfoFlags.NONE, 0);
				size = (int64) info.get_attribute_uint64 (FileAttribute.STANDARD_SIZE);
			} catch (IOError e_query) {
				warning ("Cannot query file size, continuing with an unknown size.");
			}

			FileInputStream input = yield remote.read_async ();
			FileOutputStream output;
			int64 downloaded_size = 0;

			if (input.can_seek () && local.query_exists ()) {
				output = yield local.append_to_async (FileCreateFlags.NONE, 0);
				output.seek (0, SeekType.END);
				downloaded_size = output.tell ();
				input.seek (downloaded_size, SeekType.SET);
			} else {
				output = yield local.replace_async (null, false, FileCreateFlags.NONE, 0);
			}

			uint8[] buf = new uint8[4096];

			ssize_t read = yield input.read_async (buf);
			while (read != 0) {
				yield output.write_async (buf[0:read]);
				if (on_progress != null)
					on_progress (downloaded_size + read, size);
				read = yield input.read_async (buf);
			}

		} catch (Error e) {
			set_error (ErrorEnum.NETWORK_ERROR,
				   _("Unable to download feed %s. Message: %s").printf (remote.get_basename (), e.message));
		}
		if (loop != null)
			loop.quit ();
	}

	private bool download_file_sync (string remote_url, string local_name) {
		File local_file = File.new_for_path (local_name);
		File remote_file = File.new_for_uri (remote_url);

		MainLoop main_loop = new MainLoop ();
		do_download.begin (remote_file, local_file, main_loop, null);
		main_loop.run ();
		if (last_error == null)
			return true;
		else
			return false;
	}

	private bool archive_copy_data (Archive.Read source, Archive.Write dest) {
		const int size = 10240;
		char buff[10240];
		ssize_t readBytes;

		readBytes = source.read_data (buff, size);
		while (readBytes > 0) {
			dest.write_data(buff, readBytes);
			if (dest.errno () != Archive.Result.OK) {
				emit_warning ("Error while extracting..." + dest.error_string () + "(error nb =" + dest.errno ().to_string () + ")");
				return false;
			}
			readBytes = source.read_data (buff, size);
		}
		return true;
	}

	private bool extract_entry_to (Archive.Read ar, Archive.Entry e, string dest) {
		bool ret = false;
		assert (ar != null);

		// Create new writer
		Archive.WriteDisk writer = new Archive.WriteDisk ();
		writer.set_options (Archive.ExtractFlags.SECURE_NODOTDOT | Archive.ExtractFlags.NO_OVERWRITE);

		// Change dir to extract to the right path
		string old_cdir = Environment.get_current_dir ();
		Environment.set_current_dir (dest);

		Archive.Result header_response = writer.write_header (e);
		if (header_response == Archive.Result.OK) {
			ret = archive_copy_data (ar, writer);
		} else {
			emit_warning (_("Could not extract file! Error: %s").printf (writer.error_string ()));
		}

		// Restore working dir
		Environment.set_current_dir (old_cdir);

		return ret;
	}

	private bool extract_depfile (Archive.Read ar, Archive.Entry e, string dest, Dependency dep) {
		bool ret = true;

		// Target filename
		string fname = Path.build_filename (dest, e.pathname (), null);

		// Check if file already exists
		if (FileUtils.test (fname, FileTest.EXISTS)) {
			// TODO
			//! rollback_extraction ();
			// Throw error and exit
			set_error (ErrorEnum.FILE_EXISTS,
				_("Could not override file %s, this file already exists!").printf (fname));
				return false;
		}
		// Now extract it!
		create_dir_structure (dest);
		ret = extract_entry_to (ar, e, dest);
		if ((!ret) && (Path.get_basename (fname) != ".")) {
			// TODO
			//! rollback_extraction ();
			set_error (ErrorEnum.DEPENDENCY_INSTALL_FAILED,
				   _("Unable to extract file '%s' for dependency '%s'!").printf (Path.get_basename (fname), dep.metainfo.name));
			return false;
		}

		return true;
	}

	private bool install_archive (string fname, Dependency dep) {
		if (!FileUtils.test (fname, FileTest.EXISTS))
			return false;

		Archive.Read ar = new Archive.Read ();
		// Enable support for all compressions LibArchive supports
		ar.support_format_all ();
		ar.support_compression_all ();
		if (ar.open_filename (fname, 4096) != Archive.Result.OK) {
			// The archive could not be opened
			set_error (ErrorEnum.UNPACKING_FAILED,
				_("Could not read dependency archive! Archive might be damaged. Message: %s").printf (ar.error_string ()));
			return false;
		}

		// Target dependency subdirectory (from DEP installation-var)
		string dest = dep.get_install_dir_for_setting (ssettings);

		weak Archive.Entry e;
		bool ret = false;
		while (ar.next_header (out e) == Archive.Result.OK) {
			ret = extract_depfile (ar, e, dest, dep);
			if (!ret)
				break;
		}

		if (ret) {
			HashSet<string> libs = find_files_matching (dest, "*.so*", true);
			if ((libs != null) && (libs.size > 0)) {
				string libname = libs.to_array ()[0];
				dep.environment = "LD_LIBRARY_PATH=\"%s\"".printf (Path.get_dirname (libname));
			}

		}

		return ret;
	}

	public bool install_dependency (Dependency dep) {
		if (dep.feed_url == "")
			return false;

		bool ret = false;
		string feed_file = Path.build_filename (tmpdir, Path.get_basename (dep.feed_url), null);
		// First, fetch the dependency
		ret = download_file_sync (dep.feed_url, feed_file);

		// If file downloading fails, we can exit. The error code has alredy been set
		if (!ret)
			return false;

		feed = new Feed ();
		feed.open (feed_file);

		// Search for dependency which matches the current architecture
		ret = feed.search_matching_dependency ();
		if (!ret) {
			set_error (ErrorEnum.DEPENDENCY_MISSING,
				   _("Unable to find a matching implementation of '%s' for your system/architecture.").printf (dep.metainfo.name));
			return false;
		}

		/* Update this dependency information with fresh data from the (ZI) feed
		   This is *very* important to get a sane dependency-id! */
		feed.update_dependency_data (dep);

		string package_file = Path.build_filename (tmpdir, Path.get_basename (feed.package_url), null);
		ret = download_file_sync (feed.package_url, package_file);
		// Again, exit if download failed
		if (!ret)
			return false;

		// Install the archive to the correct dependency dir
		ret = install_archive (package_file, dep);
		// Exit if there was an error
		if (!ret)
			return false;

		// Yes! Feed installation completed, we can set this as satisfied!
		dep.add_installed_item ("feed;%s;".printf (dep.feed_url));
		dep.installed = true;

		return ret;
	}

}

} // End of namespace
