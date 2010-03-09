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
  StdCtrls, Buttons, CheckLst, HTTPSend, IniFiles, MD5, LiBasic, updexec, LCLType,
  Process, Menus, trstrings, ldunit,IconLoader, LiCommon, AppUpdate, liTypes;

type

  { TUMnForm }
  
  TAppNotes = record
  AppName: String;
  NVersion, OVersion: String;
  ID: String;
  end;

  TUMnForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckListBox1: TCheckListBox;
    InfoMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    { private declarations }
    updater: Pointer;
  public
    { public declarations }
    ANotes: Array of TAppNotes;
  end; 

var
  UMnForm: TUMnForm;

implementation

{ TUMnForm }

procedure OnNewUpdateFound(appName: PChar;id: Integer;user_data: Pointer);cdecl;
begin
 if Assigned(UMnForm) then
 with UMnForm do
 begin
  p_debug(appName);
  //CheckListBox1.Items.Add(appName);
 end;
end;

function OnRequest(mtype: TRqType;msg: PChar;user_data: Pointer): TRqResult;cdecl;
begin
 p_debug(msg);
 Result:=rqsOK;
end;

procedure TUMnForm.BitBtn1Click(Sender: TObject);
begin
UExecFm.ShowModal;
end;

procedure TUMnForm.BitBtn2Click(Sender: TObject);
begin
 li_updater_search_updates(@updater);
end;

procedure TUMnForm.CheckListBox1Click(Sender: TObject);
begin
if CheckListBox1.ItemIndex>-1 then
begin
InfoMemo.Enabled:=true;
InfoMemo.Lines.Clear;
InfoMemo.Lines.Add(rsLogUpdInfo);
//InfoMemo.Lines.Add(StringReplace(rsFilesChanged,'%f',IntToStr((ulist[CheckListBox1.ItemIndex].Count div 2)),[rfReplaceAll]));
InfoMemo.Lines.Add(StringReplace(rsUpdTo,'%v','"'+anotes[CheckListBox1.ItemIndex].NVersion+'"',[rfReplaceAll]));
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

updater:=li_updater_new;
li_updater_register_newupdate_call(@updater,@OnNewUpdateFound,nil);
li_updater_register_request_call(@updater,@OnRequest,nil);
end;

procedure TUMnForm.FormDestroy(Sender: TObject);
begin
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

