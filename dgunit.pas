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
//** This unit contains the stuff neded for installing AutoGET-IPK-types

unit dgunit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, LCLType, LCLIntf, igobase, ExtCtrls, process, HTTPSend,
  blcksock, FTPSend, LiCommon, trstrings, IconLoader;

type

  { TDGForm }

  TDGForm = class(TForm)
    BitBtn1: TBitBtn;
    FinBtn1: TBitBtn;
    GetOutPutTimer: TIdleTimer;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    PageControl1: TPageControl;
    DlProgress: TProgressBar;
    MainProgress: TProgressBar;
    Process1: TProcess;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure FinBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetOutPutTimerTimer(Sender: TObject);
  private
    { private declarations }
    HTTP: THTTPSend;FTP: TFTPSend; //FTP and HTTP controls
    op, om: Integer;
    procedure HookSock(Sender: TObject; Reason:THookSocketReason; const Value: string); //Hook to HTTP and FTP socket
  public
    { public declarations }
    //** Path to iconfiles
    IIconPath: String;
    //** Path to .desktop files
    IDesktopFiles: String;
  end; 

var
  DGForm: TDGForm;

implementation

{ TDGForm }

procedure TDGForm.HookSock(Sender: TObject; Reason: THookSocketReason;
const Value: string);
begin
Application.ProcessMessages;
//HTTP
if (Http.Document.Size>120) then begin
  if op=-1 then begin
  om:=MainProgress.Max;
  op:=MainProgress.Position;
  end;

  DLProgress.Max:=HTTP.DownloadSize;

  DLProgress.Position:=HTTP.Document.Size;

  MainProgress.Position:=op+Http.Document.Size;
  MainProgress.Max:=HTTP.DownloadSize+om;
  exit;
 end;
//FTP
if FTP.DSock.RecvCounter>100 then
 DLProgress.Position:=FTP.DSock.RecvCounter;
end;

procedure TDGForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Application.Terminate;
  DGForm.Free;
end;

procedure TDGForm.BitBtn1Click(Sender: TObject);
begin
 BitBtn1.Enabled:=false;
 TabSheet3.TabVisible:=true;
 TabSheet2.TabVisible:=false;
 TabSheet1.TabVisible:=false;
 PageControl1.ActivePageIndex:=3;
 HTTP := THTTPSend.Create;
 HTTP.Sock.OnStatus:=HookSock;
 HTTP.KeepAlive:=true;
 FTP := TFTPSend.Create;
 FTP.DSock.OnStatus:=HookSock;
 //???
 //setup.HTTPSend:=HTTP;
 //setup.FTPSend:=FTP;
 GetOutPutTimer.Enabled:=true;
 //setup.DoInstallation(Process1,Memo3.Lines);
 GetOutPutTimer.Enabled:=false;

 HTTP.Free;
FTP:=nil;
FTP.Free;

PageControl1.Visible:=false;

//???
{if (FileExists(setup.AppIcon))and(
(LowerCase(ExtractFileExt(setup.AppIcon))='.png')or
(LowerCase(ExtractFileExt(setup.AppIcon))='.bmp')or
(LowerCase(ExtractFileExt(setup.AppIcon))='.jpg')) then
begin
Image1.Picture.Clear;
Image1.Picture.LoadFromFile(setup.AppIcon);
Image1.Repaint;
end; }

Label2.Visible:=false;
BitBtn1.Visible:=false;

end;

procedure TDGForm.FinBtn1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TDGForm.FormCreate(Sender: TObject);
begin
LoadStockPixmap(STOCK_EXECUTE,ICON_SIZE_BUTTON,BitBtn1.Glyph);
Image1.Picture.LoadFromFile(GetDataFile('graphics/spackage.png'));
PageControl1.ActivePageIndex:=0;
BitBtn1.Caption:=rsInstallNow;
end;

procedure TDGForm.FormShow(Sender: TObject);
begin
  IWizFrm.Hide;
  PageControl1.Visible:=true;
  TabSheet1.Caption:=rsMain;
  TabSheet2.Caption:=rsDetails;
  TabSheet3.Caption:=rsInstallation;
  
  FinBtn1.Caption:=rsFinish;
  LoadStockPixmap(STOCK_QUIT,ICON_SIZE_BUTTON,FinBtn1.Glyph);
end;

procedure TDGForm.GetOutPutTimerTimer(Sender: TObject);
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
        writeLn(OutputMemo.Text + copy(Buffer,1, BytesRead));
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

    DoStuffForProcess(Process1, Memo3);
  until noMoreOutput;
if Process1.ExitStatus>0 then begin
    GetOutputTimer.Enabled:=false;
    ShowMessage(rsCouldntSolve+#13+StringReplace(rsViewLog,'%p',ConfigDir,[rfReplaceAll]));
    Memo3.Lines.SaveTofile(ConfigDir+'/install.log');
    Application.Terminate;
    exit;
  end;
end;

initialization
  {$I dgunit.lrs}

end.

