{ Copyright (C) 2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This library is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as publishedf by the Free Software
  Foundation, version 3.

  This library is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this library. If not, see <http://www.gnu.org/licenses/>.}
//** Management of 3rd-party shared libraries
unit slibmanage;

{$mode objfpc}{$H+}

interface

uses
  Classes, LiUtils, SysUtils, Process, TarArchive;

type
  TDEBConverter = class
  private
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;

    function Unpack: Integer;
    property FileName: String read FName write FName;
  end;

  TLibManager = class
  private
  public
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TDEBConverter }

constructor TDEBConverter.Create;
begin

end;

destructor TDEBConverter.Destroy;
begin

end;

function TDEBConverter.Unpack: Integer;
begin
  {def extract_deb(stream, destdir, extract = None, start_offset = 0):
	if extract:
		raise SafeException(_('Sorry, but the "extract" attribute is not yet supported for Debs'))

	stream.seek(start_offset)
	# ar can't read from stdin, so make a copy...
	deb_copy_name = os.path.join(destdir, 'archive.deb')
	deb_copy = file(deb_copy_name, 'w')
	shutil.copyfileobj(stream, deb_copy)
	deb_copy.close()

	data_tar = None
	p = subprocess.Popen(('ar', 't', 'archive.deb'), stdout=subprocess.PIPE, cwd=destdir, universal_newlines=True)
	o = p.communicate()[0]
	for line in o.split('\n'):
		if line == 'data.tar':
			data_compression = None
		elif line == 'data.tar.gz':
			data_compression = 'gzip'
		elif line == 'data.tar.bz2':
			data_compression = 'bzip2'
		elif line == 'data.tar.lzma':
			data_compression = 'lzma'
		else:
			continue
		data_tar = line
		break
	else:
		raise SafeException(_("File is not a Debian package."))

	_extract(stream, destdir, ('ar', 'x', 'archive.deb', data_tar))
	os.unlink(deb_copy_name)
	data_name = os.path.join(destdir, data_tar)
	data_stream = file(data_name)
	os.unlink(data_name)
	extract_tar(data_stream, destdir, None, data_compression)
    }
end;

{ TLibManager }

constructor TLibManager.Create;
begin

end;

destructor TLibManager.Destroy;
begin

end;

end.

