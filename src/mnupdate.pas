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
unit mnupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, CheckLst, HTTPSend, IniFiles, MD5, LiBasic, updexec,
  LCLType, Process, Menus, ComCtrls, trstrings, IconLoader, LiCommon,
  AppUpdate, liTypes;

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
  end; 

var
  UMnForm: TUMnForm;

 //Published, so update display can access it
 procedure OnMNStatus(change: LiStatusChange;data: TLiStatusData;user_data: Pointer);cdecl;

implementation

{ TUMnForm }

procedure OnNewUpdateFound(appName: PChar;id: Integer;user_data: Pointer);cdecl;
begin
 if Assigned(UMnForm) then
 with UMnForm do
 begin
  UpdListBox.Items.Add(appName);
  UpdateIDs.Add(IntToStr(id));;
 end;
end;

function OnRequest(mtype: TRqType;msg: PChar;user_data: Pointer): TRqResult;cdecl;
begin
case mtype of
rqError: begin
  Application.MessageBox(msg,'Error',MB_OK+MB_IconError);
  Result:=rqsOK;
end;
rqWarning: begin
  if Application.MessageBox(PAnsiChar(msg),PChar(rsWarning),MB_YESNO+MB_IconWarning)<>IDYES then
  begin
   ShowMessage(rsINClose);
   Result:=rqsNo;
  end else Result:=rqsYes;
end;
rqQuestion: begin
  if Application.MessageBox(PAnsiChar(msg),PChar(rsQuestion),MB_YESNO+MB_IconQuestion)<>IDYES then
   Result:=rqsNo else Result:=rqsYes;
end;
rqInfo: begin
  ShowMessage(msg);
  Result:=rqsOK;
 end;
end;
end;

procedure OnMNStatus(change: LiStatusChange;data: TLiStatusData;user_data: Pointer);cdecl;
begin
 case change of
  scMessage: p_info(data.msg);
 end;
end;

procedure TUMnForm.BitBtn1Click(Sender: TObject);
begin
 UExecFm.ShowModal;
end;

procedure TUMnForm.BitBtn2Click(Sender: TObject);
begin
 ProgressBar.Position:=0;
 Application.ProcessMessages;
 li_updater_search_updates(@updater);
 if UpdListBox.Items.Count>0 then
  BitBtn1.Enabled:=true;
 ProgressBar.Position:=100;
 BitBtn2.Enabled:=false;
end;

procedure TUMnForm.UpdListBoxClick(Sender: TObject);
begin
if UpdListBox.ItemIndex>-1 then
begin
InfoMemo.Enabled:=true;
InfoMemo.Lines.Clear;
InfoMemo.Lines.Add(rsLogUpdInfo);
//InfoMemo.Lines.Add(StringReplace(rsFilesChanged,'%f',IntToStr((ulist[UpdListBox.ItemIndex].Count div 2)),[rfReplaceAll]));
InfoMemo.Lines.Add(StringReplace(rsUpdTo,'%v','"'+
 li_updater_updateid_newversion(@updater,StrToInt(updateids[UpdListBox.ItemIndex]))+'"',[rfReplaceAll]));
end;
end;

function IsCommandRunning(cmd:String):Boolean;
var t:TProcess;
s:TStringList;
begin
 Result:=false;
 t:=tprocess.create(nil);
 t.CommandLine:='ps -A'+cmd;
 t.Options:=[poUsePipes,poWaitonexit];
 try
  t.Execute;
  s:=tstringlist.Create;
  try
   s.LoadFromStream(t.Output);
   Result:=Pos(cmd,s.Text)>0;
  finally
  s.free;
  end;
 finally
 t.Free;
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
LoadStockPixmap(STOCK_REFRESH,ICON_SIZE_BUTTON,BitBtn2.Glyph);
LoadStockPixmap(STOCK_APPLY,ICON_SIZE_BUTTON,BitBtn1.Glyph);
//Translation
BitBtn2.Caption:=rsCheckForUpd;
BitBtn1.Caption:=rsInstUpd;
MenuItem1.Caption:=rsQuitUpdater;
MenuItem2.Caption:=rsShowUpdater;
UpdateIDs:=TStringList.Create;

updater:=li_updater_new;
li_updater_register_newupdate_call(@updater,@OnNewUpdateFound,nil);
li_updater_register_request_call(@updater,@OnRequest,nil);
li_updater_register_status_call(@updater,@OnMNStatus,nil);
end;

procedure TUMnForm.FormDestroy(Sender: TObject);
begin
  UpdateIDs.Free;
  li_updater_free(@updater);
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

initialization
  {$I mnupdate.lrs}

end.

