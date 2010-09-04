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
//** Executes software updates (GUI based)
unit updexecfrm;

{$mode objfpc}{$H+}

interface

uses
  LiAppUpdate, Classes, ComCtrls, Controls, Dialogs, FileUtil, Forms, Graphics,
  LiUtils, LiTypes, LResources, Process, StdCtrls, SysUtils, StrLocale;

type

  { TUExecFm }

  TUExecFm = class(TForm)
    Button1: TButton;
    ILabel: TLabel;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    fstact: boolean;
  public
    { public declarations }
  end;

var
  UExecFm: TUExecFm;

implementation

{$R updexecfrm.lfm}

uses updatefrm;

{ TUExecFm }

procedure OnEXStatus(change: LiStatusChange; data: LiStatusData;
  user_data: Pointer); cdecl;
begin
  Application.ProcessMessages;
  if not Assigned(UExecFm) then
  begin
    perror('UExecForm not created!');
    exit;
  end;
  with UExecFm do
  begin
    case change of
      scMessage:
      begin
        pinfo(Data.msg);
        Memo1.Lines.Add(Data.msg);
      end;
      scStepMessage: ILabel.Caption := Data.msg;
      scMnProgress: ProgressBar2.Position := Data.mnprogress;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TUExecFm.FormShow(Sender: TObject);
begin
end;

procedure TUExecFm.Button1Click(Sender: TObject);
begin
  Close;
  UMnForm.BitBtn2Click(nil);
end;

procedure TUExecFm.FormActivate(Sender: TObject);
var
  i: integer;
  strID: string;
begin
  if fstact then
  begin
    fstact := false;
    li_updater_register_status_call(@UMnForm.updater, @OnEXStatus, nil);
    li_updater_register_status_call(@UMnForm.updaterSU, @OnEXStatus, nil);

    for i := 0 to UMnForm.UpdListBox.Count - 1 do
      if UMnForm.UpdListBox.Checked[i] then
        ProgressBar1.Max := ProgressBar1.Max + 1;

    for i := 0 to UMnForm.UpdListBox.Count - 1 do
      if UMnForm.UpdListBox.Checked[i] then
      begin
        strID := UMnForm.UpdateIDs[i];
        if pos('(su)', strID) > 0 then
        begin
          strID := StringReplace(UMnForm.UpdateIDs[i], ' (su)', '', [rfReplaceAll]);
          li_updater_execute_update(@UMnForm.updaterSU, StrToInt(strID));
        end
        else
        begin
          li_updater_execute_update(@UMnForm.updater, StrToInt(strID));
        end;
        ProgressBar1.Position := ProgressBar1.Position + 1;
      end;
    li_updater_register_status_call(@UMnForm.updater, @updatefrm.OnMNStatus, nil);
    li_updater_register_status_call(@UMnForm.updaterSU, @updatefrm.OnMNStatus, nil);
    UMnForm.UpdListBox.Items.Clear;
    UMnForm.UpdateIDs.Clear;
    Button1.Enabled := true;
  end;
end;

procedure TUExecFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fstact := true;
  Button1.Enabled := false;
end;

procedure TUExecFm.FormCreate(Sender: TObject);
begin
  Memo1.Font.Color := clWhite;
  Caption := rsUpdInstalling;
  Button1.Caption := rsClose;
  Button1.Enabled := false;
  fstact := true;
end;

end.

