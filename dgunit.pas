{ dgunit.pas
  Copyright (C) Listaller Project 2008-2009

  dgunit.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  dgunit.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains the stuff neded for installing AutoGET-IPK-types

unit dgunit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, LCLType, LCLIntf, imainunit, ExtCtrls, process, HTTPSend,
  blcksock, FTPSend, IniFiles, common, trstrings, ipkhandle, packagekit;

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
var i: Integer;cnf,ar: TIniFile;pkit: TPackageKit;
begin
BitBtn1.Enabled:=false;
TabSheet3.TabVisible:=true;
TabSheet2.TabVisible:=false;
TabSheet1.TabVisible:=false;
PageControl1.ActivePageIndex:=3;
MainProgress.Max:=(Memo2.Lines.Count-1)*6000;

HTTP := THTTPSend.Create;
HTTP.Sock.OnStatus:=HookSock;
HTTP.KeepAlive:=true;
HTTP.UserAgent:='Listaller-GET';

FTP := TFTPSend.Create;
FTP.DSock.OnStatus:=HookSock;

//Set Proxy-Settings
cnf:=TInifile.Create(ConfigDir+'config.cnf');
if cnf.ReadBool('Proxy','UseProxy',false) then begin
//Set HTTP
HTTP.ProxyPort:=cnf.ReadString('Proxy','hPort','');
HTTP.ProxyHost:=cnf.ReadString('Proxy','hServer','');
HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
HTTP.ProxyPass:=cnf.ReadString('Proxy','Password',''); //The PW is visible in the file! It should be crypted
//Not needed
{if DInfo.Desktop='GNOME' then begin
HTTP.ProxyPass:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_user');
HTTP.ProxyUser:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_password');
 end;}
end;

ShowPKMon();

Application.ProcessMessages;
BitBtn1.Enabled:=false;
pkit:=TPackageKit.Create;

  for i:=1 to Memo2.Lines.Count-1 do begin
  if pos('://',Memo2.Lines[i])<=0 then begin
  Memo3.Lines.Add('Looking for '+Memo2.Lines[i]);
  if not pkit.IsInstalled(Memo2.Lines[i]) then begin
  Memo3.Lines.Add('Installing '+Memo2.Lines[i]+'...');
  GetOutPutTimer.Enabled:=true;
  pkit.InstallPkg(Memo2.Lines[i]);
  end;
end else begin
  Memo3.Lines.Add('Looking for '+copy(Memo2.Lines[i],1,pos(' -',Memo2.Lines[i])-1));
  if not pkit.IsInstalled(copy(Memo2.Lines[i],1,pos(' -',Memo2.Lines[i])-1)) then begin
  Memo3.Lines.Add('Downloading package...');
  
if pos('http://',LowerCase(Memo2.Lines[i]))>0 then begin
 op:=-1;
 try
  HTTP.HTTPMethod('GET', copy(Memo2.Lines[i],pos(' -',Memo2.Lines[i])+2,length(Memo2.Lines[i])-pos(' -',Memo2.Lines[i])+2));
  HTTP.Document.SaveToFile('/tmp/'+ExtractFileName(Memo2.Lines[i]));
 except
   ShowMessage(strDepDlProblem);
   Application.Terminate;
   exit;
  end;

  end else begin
  with FTP do begin
    TargetHost := GetServerName(Memo2.Lines[i]);
  try
    DirectFileName := '/tmp/'+ExtractFileName(Memo2.Lines[i]);
    DirectFile:=True;
    if not Login then ShowMessage(strFTPfailed);
    ChangeWorkingDir(GetServerPath(Memo2.Lines[i]));

    IWizFrm.ExProgress.Max:=FileSize(ExtractFileName(Memo2.Lines[i]));

    RetrieveFile(ExtractFileName(Memo2.Lines[i]), false);
    Logout;
  except
   ShowMessage(strDepDlProblem);
   Application.Terminate;
   exit;
  end;
  end;
end;

  Memo3.Lines.Add('Installing '+copy(Memo2.Lines[i],1,pos(' -',Memo2.Lines[i])-1)+'...');
  GetOutPutTimer.Enabled:=true;
  pkit.InstallLocalPkg('/tmp/'+ExtractFileName(Memo2.Lines[i]));
end;

  
  MainProgress.Position:=MainProgress.Position+6000;
end;

end;

pkit.Free;

with IWizFrm do begin
pID:='{101-101-101-101}';

while Length(IDesktopFiles)>1 do begin
if pos(';',IDesktopFiles)>0 then
ar:=TInifile.Create('/usr/share/applications/'+copy(IDesktopFiles,0,pos(';',IDesktopFiles)-1))
else ar:=TInifile.Create('/usr/share/applications/'+IDesktopFiles);
ShowMessage(copy(IDesktopFiles,0,pos(';',IDesktopFiles)-1)+' # '+IDesktopFiles);
ar.WriteString('Desktop Entry','X-AppVersion',IAppVersion);
ar.WriteString('Desktop Entry','X-Publisher',IAuthor);
if pos(';',IDesktopFiles)>0 then
IDesktopFiles:=copy(IDesktopFiles,pos(';',IDesktopFiles)+1,length(IDesktopFiles))
else IDesktopFiles:='';
ar.Free;
end;
end;
{for i:=1 to Memo2.Lines.Count-1 do begin
if pos('://',Memo2.Lines[i])<=0 then
ar.WriteString('DepOS','ID'+IntToStr(i),Memo2.Lines[i])
else
ar.WriteString('DepOS','ID'+IntToStr(i),copy(Memo2.Lines[i],1,pos(' -',Memo2.Lines[i])-1));
end; }

HTTP.Free;
FTP:=nil;
FTP.Free;

PageControl1.Visible:=false;

if (FileExists(lp+IWizFrm.PkgName+IIconPath))and(
(LowerCase(ExtractFileExt(IIconPath))='.png')or
(LowerCase(ExtractFileExt(IIconPath))='.bmp')or
(LowerCase(ExtractFileExt(IIconPath))='.jpg')) then begin
Image1.Picture.Clear;
ShowMessage(lp+IWizFrm.PkgName+IIconPath);
Image1.Picture.LoadFromFile(lp+IWizFrm.PkgName+IIconPath);
Image1.Repaint;
end;

Label2.Visible:=false;
BitBtn1.Visible:=false;
ShowMessage(strSuccess);
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
BitBtn1.Caption:=strInstallNow;
end;

procedure TDGForm.FormShow(Sender: TObject);
begin
  IWizFrm.Hide;
  PageControl1.Visible:=true;
  TabSheet1.Caption:=strMain;
  TabSheet2.Caption:=strDetails;
  TabSheet3.Caption:=strInstallation;
  
  FinBtn1.Caption:=strFinish;
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
    ShowMessage(strCouldntSolve+#13+StringReplace(strViewLog,'%p',ConfigDir,[rfReplaceAll]));
    Memo3.Lines.SaveTofile(ConfigDir+'/install.log');
    Application.Terminate;
    exit;
  end;
end;

initialization
  {$I dgunit.lrs}

end.

