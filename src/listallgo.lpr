{ Copyright (C) 2008-2010 Matthias Klumpp

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
//** GUI wizard application for IPK package installations
program listallgo;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX}
  cthreads, {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  gExt,
  Forms,
  igobase,
  dgfrm,
  trStrings,
  SysUtils,
  LiBasic,
  liTranslator,
  liTypes,
  xtypefm,
  Installer,
  LCLType,
  Dialogs;

{$R listallgo.res}

begin
  //Init application
  Application.Title := 'Software Installer';
  Application.Initialize;
  //Make the GType system work for us
  InitializeGType();
  writeLn('Application initialized.');

  //Load IPK-File, quit on failure
  if LoadIPKFile() then
  begin
    { --- Do Container setup --- }
    if setup.PkType = ptContainer then
    begin
      if not setup.StartInstallation then
        ShowMessage(rsInstFailed);
      setup.Free;
      Application.Terminate;
      exit;
    end
    else

    { --- Prepare Listallation --- }
      if setup.PkType = ptLinstall then
      begin
        //Check if app is already installed
        if not Testmode then
        begin
          if IsIPKAppInstalled(setup.GetAppName, setup.GetAppID, Superuser) then
            if Application.MessageBox(PAnsiChar(PAnsiChar(rsAlreadyInst) + #13 +
              PAnsiChar(rsInstallAgain)), PAnsiChar(rsReInstall), MB_YESNO) = idNo then
            begin
              setup.Free;
              Application.Terminate;
              exit;
            end;
        end;
        Application.CreateForm(TIWizFrm, IWizFrm);
        Application.UpdateMainForm(IWizFrm);
      end
      else
      { --- Prepare DLink Install --- }
        if setup.PkType = ptDLink then
        begin
          Application.CreateForm(TDGForm, DGForm);

          DGForm.IIconPath := setup.GetAppIcon;
          DGForm.IDesktopFiles := setup.GetDesktopFiles;
          with DGForm do
          begin
            Label1.Caption := StringReplace(rsInstOf, '%a', setup.GetAppName, [rfReplaceAll]);
            Label2.Caption := rsWillDLFiles;
            Caption := Label1.Caption;
            Memo2.Clear;
            Memo2.Lines.Add(rsPkgDownload);
          end;
          Application.UpdateMainForm(DGForm);
        end;


    Application.ShowMainForm := true;
    writeLn('GUI created.');
    Application.Run;
    writeLn('Installer closed.');
  end
  else
    Application.Terminate;

end.

