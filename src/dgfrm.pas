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
//** This unit contains the stuff neded for installing AutoGET-IPK-types

unit dgfrm;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, LCLType, LCLIntf, igobase, ExtCtrls, process, liUtils,
  strLocale, IconLoader;

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

{$R dgfrm.lfm}

{ TDGForm }

procedure TDGForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Application.Terminate;
end;

procedure TDGForm.BitBtn1Click(Sender: TObject);
begin
 BitBtn1.Enabled:=false;
 FinBtn1.Visible:=false;
 TabSheet3.TabVisible:=true;
 TabSheet2.TabVisible:=false;
 TabSheet1.TabVisible:=false;
 PageControl1.ActivePageIndex:=3;
 {HTTP := THTTPSend.Create;
 HTTP.Sock.OnStatus:=HookSock;
 HTTP.KeepAlive:=true;
 FTP := TFTPSend.Create;
 FTP.DSock.OnStatus:=HookSock;
 GetOutPutTimer.Enabled:=true;}
 setup.StartInstallation;

{ HTTP.Free;
 FTP:=nil;
 FTP.Free; }

 PageControl1.Visible:=false;

if (FileExists(setup.GetAppIcon))and(
(LowerCase(ExtractFileExt(setup.GetAppIcon))='.png')or
(LowerCase(ExtractFileExt(setup.GetAppIcon))='.bmp')or
(LowerCase(ExtractFileExt(setup.GetAppIcon))='.jpg')) then
begin
Image1.Picture.Clear;
Image1.Picture.LoadFromFile(setup.GetAppIcon);
Image1.Repaint;
end;

Label2.Visible:=false;
BitBtn1.Visible:=false;
FinBtn1.Visible:=true;
end;

procedure TDGForm.FinBtn1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TDGForm.FormCreate(Sender: TObject);
var tmp: tstringList;i: Integer;
begin
LoadStockPixmap(STOCK_EXECUTE,ICON_SIZE_BUTTON,BitBtn1.Glyph);
Image1.Picture.LoadFromFile(GetDataFile('graphics/spackage.png'));
PageControl1.ActivePageIndex:=0;
BitBtn1.Caption:=rsInstallNow;

//Load deps
tmp:=TStringList.Create;
setup.ReadDeps(tmp);
for i:=0 to tmp.Count-1 do
begin
 Memo2.Lines.Add(tmp[i]);
end;
Memo2.Update;
tmp.Free;

setup.ReadLongDescription(TStringList(Memo1.Lines));

TabSheet1.Caption:=rsMain;
TabSheet2.Caption:=rsDetails;
TabSheet3.Caption:=rsInstallation;

FinBtn1.Caption:=rsFinish;
LoadStockPixmap(STOCK_QUIT,ICON_SIZE_BUTTON,FinBtn1.Glyph);
TabSheet3.TabVisible:=false;
end;

procedure TDGForm.FormShow(Sender: TObject);
begin
  PageControl1.Visible:=true;
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

end.

