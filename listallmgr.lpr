{ listallmngr.lpr
  Copyright (C) Listaller Project 2008-2009

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
program listallmgr;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  manager, uninstall, pkgconvertdisp, swcatalog,
  LiCommon, LiTranslator, LResources, Process;

{$IFDEF WINDOWS}{$R listallmgr.rc}{$ENDIF}

var
 p: TProcess;
begin
  {$I listallmgr.lrs}
  //Submit IPK packages to listallgo and quit.
  //If no package exists, open Listaller Manager surface
  if FileExists(paramstr(1)) then
  begin
   p:=TProcess.Create(nil);
   p.Options:=[];
   p.CommandLine := ExtractFilePath(Application.ExeName)+'listallgo '+paramstr(1);
   p.Execute;
   p.Free;
   halt(0);
   exit;
  end else
  begin
  Application.Title:='Listaller Manager';
  Application.ShowMainForm:=false;
  Application.Initialize;
  writeLn('Application initialized.');
  Application.CreateForm(TMnFrm, MnFrm);
  Application.CreateForm(TConvDisp, ConvDisp);
  Application.CreateForm(TSCForm, SCForm);
  Application.Run;
  writeLn('Listaller manager closed.');
  end;
end.
