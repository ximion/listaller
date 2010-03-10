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
unit updexec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, HTTPSend, FileUtil, Process, LiCommon, trStrings, LiBasic, AppUpdate;

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
    fstact: Boolean;
    //** Hook on HTTP/FTP socket
     //procedure HookSock(Sender: TObject; Reason:THookSocketReason; const Value: string);
  public
    { public declarations }
  end; 

var
  UExecFm: TUExecFm;

implementation

uses mnupdate;

{ TUExecFm }

procedure TUExecFm.FormShow(Sender: TObject);
begin
end;

procedure TUExecFm.Button1Click(Sender: TObject);
begin
  Close;
  UMnForm.BitBtn2Click(nil);
end;
    
{procedure TUExecFm.HookSock(Sender: TObject; Reason: THookSocketReason;
const Value: string);
begin
if HTTP.DownloadSize>100 then begin
ProgressBar2.Max:=HTTP.DownloadSize;
ProgressBar2.Position:=HTTP.Document.Size;
end;
Application.ProcessMessages;
end;}

procedure TUExecFm.FormActivate(Sender: TObject);
var i: Integer;
begin
 if fstact then
 begin
  fstact:=false;
  for i:=0 to UMnForm.UpdListBox.Count-1 do
   if UMnForm.UpdListBox.Checked[i] then
    ProgressBar1.Max:=ProgressBar1.Max+1;

  for i:=0 to UMnForm.UpdListBox.Count-1 do
   if UMnForm.UpdListBox.Checked[i] then
   begin
    li_updater_execute_update(@UMnForm.updater,StrToInt(UMnForm.UpdateIDs[i]));
    ProgressBar1.Position:=ProgressBar1.Position+1;
   end;
 end;
end;

procedure TUExecFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fstact:=true;
end;

procedure TUExecFm.FormCreate(Sender: TObject);
begin
  Memo1.Font.Color:=clWhite;
  Caption:=rsUpdInstalling;
  Button1.Caption:=rsClose;
  fstact:=true;
end;

initialization
  {$I updexec.lrs}

end.

