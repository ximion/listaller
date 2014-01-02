/* lilith-factory.vala -- Stuff to provide checks easy access to the current package internals
 *
 * Copyright (C) 2012-2014 Matthias Klumpp <matthias@tenstral.net>
 *
 * Licensed under the GNU General Public License Version 3
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
 */

using GLib;
using Listaller;

public interface ICheckInterface : Object {
    public abstract CheckFactory factory { get; set; }
    public abstract void run_check ();
}

public class CheckFactory : Object {
	private string ipkFName;

	public CheckFactory (string ipk_fname) {
		ipkFName = ipk_fname;
	}
}
