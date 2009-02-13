{ listallmngr.lpr
  Copyright (C) Listaller Project 2008

  listallmngr.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  listallmngr.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program.  If not, see <http://www.gnu.org/licenses/>}
//** Application that manages all installed applications
program listallmngr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  manager, settings, uninstall, pkgconvertdisp, swcatalog, LResources, ipkhandle;

{$IFDEF WINDOWS}{$R listallmngr.rc}{$ENDIF}

begin
  {$I listallmngr.lrs}
  Application.Title:='Listaller Manager';
  Application.ShowMainForm:=false;
  Application.Initialize;
  pkit := ExtractFilePath(Application.ExeName)+'pkitbind/pkitbind.py ';
  writeLn('Application initialized.');
  Application.CreateForm(TmnFrm, mnFrm);
  Application.CreateForm(TFmConfig, FmConfig);
  Application.CreateForm(TConvDisp, ConvDisp);
  Application.CreateForm(TSCForm, SCForm);
  Application.Run;
  writeLn('Listaller manager closed.');
end.
