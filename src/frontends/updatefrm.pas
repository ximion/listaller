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
//** Main unit of the updater application
unit updatefrm;

{$mode objfpc}{$H+}

interface

uses
  Forms, Menus, Buttons, Classes, Dialogs, LCLType, liTypes,
  LiUtils, Process, CheckLst, ComCtrls, Controls, ExtCtrls,
  Graphics, StdCtrls, SysUtils, StrLocale, IconLoader,
  LResources, updexecfrm, LiAppUpdate;

type

  { TUMnForm }
  TUMnForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ProgressBar: TProgressBar;
    StatusBar1: TStatusBar;
    UpdListBox: TCheckListBox;
    InfoMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure UpdListBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    UpdateIDs: TStringList;
    updater: Pointer;
    updaterSU: Pointer;
  end;

var
  UMnForm: TUMnForm;

//Published, so update display can access it
procedure OnMNStatus(change: LiStatusChange; Data: TLiStatusData;
  user_data: Pointer); cdecl;

implementation

{$R updatefrm.lfm}

{ TUMnForm }

procedure OnNewUpdateFound(appName: PChar; id: Integer; user_data: Pointer); cdecl;
begin
  Application.ProcessMessages;
  if Assigned(UMnForm) then
    with UMnForm do
    begin
      UpdListBox.Items.Add(appName);
      UpdateIDs.Add(IntToStr(id));
      ;
    end;
end;

procedure OnNewUpdateFoundSU(appName: PChar; id: Integer; user_data: Pointer); cdecl;
begin
  Application.ProcessMessages;
  if Assigned(UMnForm) then
    with UMnForm do
    begin
      UpdListBox.Items.Add(appName + ' [SU]');
      UpdateIDs.Add(IntToStr(id) + ' (su)');
      ;
    end;
end;

function OnRequest(mtype: TRqType; msg: PChar; user_data: Pointer): TRqResult; cdecl;
begin
  case mtype of
    rqError:
    begin
      Application.MessageBox(msg, 'Error', MB_OK + MB_IconError);
      Result := rqsOK;
    end;
    rqWarning:
    begin
      if Application.MessageBox(PAnsiChar(msg), PChar(rsWarning),
        MB_YESNO + MB_IconWarning) <> idYes then
      begin
        ShowMessage(rsINClose);
        Result := rqsNo;
      end
      else
        Result := rqsYes;
    end;
    rqQuestion:
    begin
      if Application.MessageBox(PAnsiChar(msg), PChar(rsQuestion),
        MB_YESNO + MB_IconQuestion) <> idYes then
        Result := rqsNo
      else
        Result := rqsYes;
    end;
    rqInfo:
    begin
      ShowMessage(msg);
      Result := rqsOK;
    end;
  end;
end;

procedure OnMNStatus(change: LiStatusChange; Data: TLiStatusData;
  user_data: Pointer); cdecl;
begin
  Application.ProcessMessages;
  case change of
    scMessage: pinfo(Data.msg);
    scMnProgress: UMnForm.ProgressBar.Position := Data.mnprogress;
  end;
end;

procedure TUMnForm.BitBtn1Click(Sender: TObject);
begin
  UExecFm.ShowModal;
end;

procedure TUMnForm.BitBtn2Click(Sender: TObject);
begin
  ProgressBar.Position := 0;
  Application.ProcessMessages;
  BitBtn2.Enabled := false;
  li_updater_search_updates(@updater);
  li_updater_search_updates(@updaterSU);
  if UpdListBox.Items.Count > 0 then
    BitBtn1.Enabled := true;
  ProgressBar.Position := 100;
end;

procedure TUMnForm.UpdListBoxClick(Sender: TObject);
var
  id: String;
begin
  if UpdListBox.ItemIndex > -1 then
  begin
    InfoMemo.Enabled := true;
    InfoMemo.Lines.Clear;
    InfoMemo.Lines.Add(rsLogUpdInfo);
    //InfoMemo.Lines.Add(StringReplace(rsFilesChanged,'%f',IntToStr((ulist[UpdListBox.ItemIndex].Count div 2)),[rfReplaceAll]));
    if pos('(su)', updateids[UpdListBox.ItemIndex]) > 0 then
    begin
      //Application is superuser app
      id := StringReplace(updateids[UpdListBox.ItemIndex], ' (su', '', [rfReplaceAll]);
      InfoMemo.Lines.Add(StringReplace(rsUpdTo, '%v', '"' +
        li_updater_updateid_newversion(@updaterSU, StrToInt(id)) + '"', [rfReplaceAll]));
    end
    else
    begin
      id := updateids[UpdListBox.ItemIndex];
      InfoMemo.Lines.Add(StringReplace(rsUpdTo, '%v', '"' +
        li_updater_updateid_newversion(@updater, StrToInt(id)) + '"', [rfReplaceAll]));
    end;
  end;
end;

procedure TUMnForm.FormCreate(Sender: TObject);
begin
  if not DirectoryExists(RegDir) then
  begin
    CreateDir(ExtractFilePath(RegDir));
    CreateDir(RegDir);
  end;

  //Set icons
  LoadStockPixmap(STOCK_REFRESH, ICON_SIZE_BUTTON, BitBtn2.Glyph);
  LoadStockPixmap(STOCK_APPLY, ICON_SIZE_BUTTON, BitBtn1.Glyph);
  //Translation
  BitBtn2.Caption := rsCheckForUpd;
  BitBtn1.Caption := rsInstUpd;
  MenuItem1.Caption := rsQuitUpdater;
  MenuItem2.Caption := rsShowUpdater;
  UpdateIDs := TStringList.Create;

  //Create normal updater
  updater := li_updater_new;
  li_updater_register_newupdate_call(@updater, @OnNewUpdateFound, nil);
  li_updater_register_request_call(@updater, @OnRequest, nil);
  li_updater_register_status_call(@updater, @OnMNStatus, nil);
  li_updater_set_sumode(@updater, false);
  //Create updater for shared su applications
  updaterSU := li_updater_new;
  li_updater_register_newupdate_call(@updaterSU, @OnNewUpdateFoundSU, nil);
  li_updater_register_request_call(@updaterSU, @OnRequest, nil);
  li_updater_register_status_call(@updaterSU, @OnMNStatus, nil);
  li_updater_set_sumode(@updaterSU, true);
end;

procedure TUMnForm.FormDestroy(Sender: TObject);
begin
  UpdateIDs.Free;
  li_updater_free(@updater);
  li_updater_free(@updaterSU);
end;

procedure TUMnForm.FormShow(Sender: TObject);
begin
  //CheckForUpdates;
end;

procedure TUMnForm.MenuItem1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TUMnForm.MenuItem2Click(Sender: TObject);
begin
  UMnForm.Show;
end;

end.

