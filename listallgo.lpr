{ listallgo.lpr
  Copyright (C) Listaller Project 2008

  listallgo.lpr is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  listallgo.lpr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** GUI wizard application for IPK package installations
program listallgo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  imainunit, dgunit, trstrings, LResources,
  ipkhandle, SysUtils, distri, common;

{$IFDEF WINDOWS}{$R listallgo.rc}{$ENDIF}

begin
  {$I listallgo.lrs}
  Application.Title:='Installer';
  Application.Initialize;
  writeLn('Application initialized.');
  if not IsRoot then RegDir:=SyblToPath('$INST')+'/app-reg/';
  Application.CreateForm(TIWizFrm, IWizFrm);
  Application.ShowMainForm:=false;
  writeLn('GUI created.');
  Application.Run;
  writeLn('Installer closed.');
end.

