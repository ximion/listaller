{ Copyright (C) 2008-2010 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation; version 3.
  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.
  You should have received a copy of the GNU General Public License v3
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Window that shows the progress while uninstalling applications
unit uninstall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, LCLType, LiBasic, Buttons, ExtCtrls, process, trStrings, FileUtil,
  appman, liTypes, PackageKit;

type

  { TRMForm }
  TRMForm = class(TForm)
    BitBtn1: TBitBtn;
    DetailsBtn: TBitBtn;
    GetOutPutTimer: TIdleTimer;
    Label1: TLabel;
    Memo1: TMemo;
    Process1: TProcess;
    UProgress: TProgressBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure DetailsBtnClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetOutPutTimerTimer(Sender: TObject);
  private
    { private declarations }
    FActiv: Boolean;
    astatus: LiProcStatus;
  public
    { public declarations }
    property RmProcStatus: LiProcStatus read astatus write astatus;
  end; 

var
 //** Window that shows the progress of the uninstallation
  RMForm: TRMForm;

implementation

{$R uninstall.lfm}

uses manager;

{ TRMForm }

procedure TRMForm.FormShow(Sender: TObject);
begin
 Label1.Caption:=rsWaiting;
 self.Caption:=StringReplace(rsRMAppC,'%a','...',[rfReplaceAll])
end;

procedure TRMForm.GetOutPutTimerTimer(Sender: TObject);
  var
  NoMoreOutput: boolean;

  procedure DoStuffForProcess(Process: TProcess;
    OutputMemo: TMemo);
  var
    Buffer: string;
    BytesAvailable: DWord;
    BytesRead:LongInt;
  begin

    if Process.Running then
    begin

      BytesAvailable := Process.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
       begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := Process.OutPut.Read(Buffer[1], BytesAvailable);
        OutputMemo.Text := OutputMemo.Text + copy(Buffer,1, BytesRead);
        Application.ProcessMessages;
        BytesAvailable := Process.OutPut.NumBytesAvailable;
        NoMoreOutput := false;
      end;
      if BytesRead>0 then
        OutputMemo.SelStart := Length(OutputMemo.Text);
    end;

  end;
begin
  repeat
    NoMoreOutput := true;
    Application.ProcessMessages;

    DoStuffForProcess(Process1, Memo1);
  until noMoreOutput;
if Process1.ExitStatus>0 then begin
    GetOutputTimer.Enabled:=false;
    ShowMessage(rsRMError);
    Memo1.Lines.SaveTofile(ConfigDir+'uninstall.log');
    halt;
    exit;
  end;
end;

procedure LogAdd(s: String);
begin
writeLn(s);
RMForm.Memo1.Lines.add(s);
end;

procedure OnRmStatus(change: LiStatusChange;data: TLiStatusData;user_data: Pointer);cdecl;
begin
 case change of
  scMessage     : LogAdd(data.msg);
  scMnProgress  : RMForm.UProgress.Position:=data.mnprogress;
  scActionStatus: RMForm.astatus:=data.lastresult;
 end;
 Application.ProcessMessages;
end;

procedure TRMForm.FormActivate(Sender: TObject);
begin
if FActiv then
begin
FActiv:=false;
if MnFrm.uApp.uID<>'' then
begin
Label1.Caption:=StringReplace(rsRMAppC,'%a',MnFrm.uApp.Name,[rfReplaceAll]);

if Application.MessageBox(PChar(StringReplace(rsRealUninstQ,'%a',MnFrm.uApp.Name,[rfReplaceAll])),'Uninstall?',MB_YESNO)=IDYES then
begin
  UProgress.Position:=0;
 li_mgr_register_status_call(@MnFrm.amgr,@OnRmStatus,nil);
 astatus:=prNone;
 BitBtn1.Enabled:=false;
 Application.ProcessMessages;
  li_mgr_remove_app(@MnFrm.amgr,MnFrm.uApp);
 while astatus=prNone do
 begin
  sleep(1);
  Application.ProcessMessages;
 end;
 li_mgr_register_status_call(@MnFrm.amgr,@manager.OnMgrStatus,nil);

 //!!!: Misterious crash appears when executing this code.
 //MnFrm.ReloadAppList(true);
 BitBtn1.Enabled:=true;
end else close;

end else
begin
 ShowMessage('Error in selection.');
 close;
 exit;
end;
end;
end;

procedure TRMForm.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TRMForm.DetailsBtnClick(Sender: TObject);
begin
  if Memo1.Visible then
  begin
   Memo1.Visible:=false;
   Width:=456;
   Height:=134;
   DetailsBtn.Caption:=rsDetails+' -> ';
  end else
  begin
   Memo1.Visible:=true;
   Width:=456;
   Height:=294;
   DetailsBtn.Caption:=rsDetails+' <- ';
  end;
end;

procedure TRMForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FActiv:=true;
end;

procedure TRMForm.FormCreate(Sender: TObject);
begin
  FActiv:=true;
  DetailsBtnClick(Sender);
end;

end.

