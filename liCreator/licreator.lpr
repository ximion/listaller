{ Copyright (C) 2008-2009 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** IPK source GUI editor & generator
program LiPkgCreator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  editor, fwiz, prjwizard, LResources;

{$IFDEF WINDOWS}{$R licreator.rc}{$ENDIF}

begin
  {$I licreator.lrs}
  Application.Initialize;
  Application.CreateForm(TFrmEditor, FrmEditor);
  Application.CreateForm(TfrmFileWizard, frmFileWizard);
  Application.CreateForm(TfrmProjectWizard, frmProjectWizard);
  Application.Run;
end.

