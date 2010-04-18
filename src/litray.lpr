{ litray.pas
  Copyright (C) Listaller Project 2008-2009

  litray.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  litray.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** liTray shows notifications abou various Listaller actions. It also performs automatic update checks
program litray;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, linotify,
  LiTranslator, gExt;

{$R *.res}

begin
  Application.Title:='Listaller TrayControl';
  Application.Initialize;
  //Make the GType system work for us
  InitializeGType();
  Application.ShowMainForm:=false;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

