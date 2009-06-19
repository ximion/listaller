{ mainunit.pas
  Copyright (C) Listaller Project 2008-2009

  mainunit.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  mainunit.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains the code for the graphical installation of standard IPK-packages
unit igobase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, IniFiles, FileUtil, ExtCtrls, process, Buttons,
  LCLType, LCLIntf, distri, LiCommon, HTTPSend, blcksock, FTPSend,
  TRStrings, translations, gettext, SynEdit, xtypefm, ipkhandle;

type

  { TIWizFrm }

  //** The installer wizard window
  TIWizFrm = class(TForm)
    AbortBtn1: TBitBtn;
    Button1: TBitBtn;
    btn_sendinput: TButton;
    Button5: TBitBtn;
    CheckBox1: TCheckBox;
    CbExecApp: TCheckBox;
    Edit1: TEdit;
    ExProgress: TProgressBar;
    FinBtn1: TBitBtn;
    GetOutPutTimer: TIdleTimer;
    LeftImg: TImage;
    InfoMemo: TMemo;
    InsProgress: TProgressBar;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    LblTestMode: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    LicMemo: TMemo;
    Notebook1: TNotebook;
    OpenDialog1: TOpenDialog;
    IMPage: TPage;
    ModeGroup: TRadioGroup;
    DSolveProgress: TProgressBar;
    WPage: TPage;
    DPage: TPage;
    LPage: TPage;
    IPage: TPage;
    FinPage: TPage;
    Panel1: TPanel;
    Process1: TProcess;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure AbortBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FinBtn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetOutputTimerTimer(Sender: TObject);
    procedure btn_sendinputClick(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    //** HTTP/FTP socket hook
    procedure HookSock(Sender: TObject; Reason:THookSocketReason; const Value: string);
    //Setup messages
    procedure setupStateMessage(Sender: TObject; msg: String);
    procedure MainMaxPosChange(Sender: TObject;max: Integer);
    procedure ExtraMaxPosChange(Sender: TObject;max: Integer);
    procedure MainPosChange(Sender: TObject;pos: Integer);
    procedure ExtraPosChange(Sender: TObject;pos: Integer);
    procedure MainVisibleChange(Sender: TObject;vis: Boolean);
    procedure ExtraVisibleChange(Sender: TObject;vis: Boolean);
    procedure InstallationError(Sender: TObject;msg: String);
    procedure ITerminateQuestion(Sender: TObject; msg: String);
    procedure DepLoadPosChange(Sender: TObject;pos: Integer);
    procedure DepLoadMaxChange(Sender: TObject;max: Integer);
    //
  private
    { private declarations }
    //** True if installation is beeing aborted
    AbortIns: Boolean;
    //** Starte the GUI inatallation
    procedure StartInstallation;
  public
    { public declarations }
  end; 

var
  IWizFrm: TIWizFrm;
  //** Distribution information
  DInfo: TDistroInfo;
  //** IPK installation object
  setup: TInstallation;

implementation

uses DGUnit;

{ TIWizFrm }

procedure TIWizFrm.Button1Click(Sender: TObject);
begin
case Notebook1.PageIndex of
5: exit;
4: exit;
3: begin
    setup.IFileInfo:='/stuff/fileinfo-'+copy(setup.Profiles[ModeGroup.ItemIndex],pos(' #',setup.Profiles[ModeGroup.ItemIndex])+2,length(setup.Profiles[ModeGroup.ItemIndex]))+'.id';
    Button5.Visible:=false;
    Button1.Visible:=false;
    NoteBook1.PageIndex:=4;
    StartInstallation;
    exit;
  end;
2: begin
        if setup.IFileInfo<>'*' then begin
           Button5.Visible:=false;
           Button1.Visible:=false;
           NoteBook1.PageIndex:=4;
           StartInstallation;
           exit;
        end;
        Button1.Caption:=strInstallNow;
        NoteBook1.PageIndex:=3;
   end;
1: begin
 if (setup.LicenseFile='')and(setup.IFileInfo<>'*') then begin
  Button5.Visible:=false;
  Button1.Visible:=false;
  NoteBook1.PageIndex:=3;
  StartInstallation;
  exit;
 end;

     if setup.IFileInfo<>'*' then begin
        Button1.Caption:=strInstallNow;
     end;

  if RadioButton2.Checked then
  Button1.Enabled:=false
  else
  Button1.Enabled:=true;

   if (setup.LicenseFile='') then begin
    Button1.Caption:=strInstallNow;
    Notebook1.PageIndex:=3 end else
    Notebook1.PageIndex:=2;
  end;
0: begin
 if (setup.DescFile='')and(setup.LicenseFile='')and(setup.IFileInfo<>'*') then begin
  Button5.Visible:=false;
  Button1.Visible:=false;
  NoteBook1.PageIndex:=3;
  StartInstallation;
  exit;
 end;
 if (setup.DescFile='') then begin
  Button1.Caption:=strInstallNow;
 if RadioButton2.Enabled then
  Button1.Enabled:=false;
  NoteBook1.PageIndex:=2;
  Button5.Visible:=true;
  exit;
 end;
 if (setup.DescFile<>'')and(setup.LicenseFile='') then begin
  NoteBook1.PageIndex:=1;
  Button5.Visible:=true;
  Button1.Caption:=strInstallNow;
  exit;
 end;
  NoteBook1.PageIndex:=1;
  Button5.Visible:=true;
  end;
end;
end;

procedure TIWizFrm.AbortBtn1Click(Sender: TObject);
begin
if NoteBook1.PageIndex=5 then begin
  Label10.Caption:=strInstAborted;
  Label11.Caption:=StringReplace(strAppNInstall,'%a',setup.AppName,[rfReplaceAll]);
  AbortIns:=true;
end else
  Application.Terminate;
end;

procedure TIWizFrm.Button5Click(Sender: TObject);
begin
case NoteBook1.PageIndex of
1: begin
  Button1.Caption:=strNext;
  Notebook1.PageIndex:=0;
  Button5.Visible:=false;
  Button1.Enabled:=true;
  end;
2: begin
  Button1.Caption:=strNext;
 { Button1.Width:=83;
  Button1.Left:=566; }
  if setup.DescFile='' then
  Notebook1.PageIndex:=0
  else
  Notebook1.PageIndex:=1;
  Button1.Enabled:=true;
  end;
3: begin
        Button1.Caption:=strNext;
        if setup.LicenseFile='' then
        Notebook1.PageIndex:=1
        else
        Notebook1.PageIndex:=2;
        Button1.Enabled:=true;
   end;
4: exit;
end;
end;

procedure TIWizFrm.CheckBox1Change(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then begin
  ListBox1.Visible:=true;
  Infomemo.Visible:=true;
  end else begin
  ListBox1.Visible:=false;
  Infomemo.Visible:=false;
  end;
end;

procedure TIWizFrm.FinBtn1Click(Sender: TObject);
begin
  FinBtn1.Enabled:=false;
  Label16.Caption:='Clean workdir...';
  Label16.Visible:=true;
  FinPage.Refresh;
  DeleteDirectory(lp+ExtractFileName(paramstr(1)),false);

  if (setup.CMDln<>'#')and(CbExecApp.Checked) then begin
  Process1.CommandLine:=setup.CMDln;
  Process1.Execute;
  end;

  Application.Terminate;
end;

procedure TIWizFrm.DepLoadMaxChange(Sender: TObject;max: Integer);
begin
 DSolveProgress.Max:=max;
end;

procedure TIWizFrm.DepLoadPosChange(Sender: TObject;pos: Integer);
begin
 DSolveProgress.Position:=pos;
 Application.ProcessMessages;
end;

procedure TIWizFrm.FormActivate(Sender: TObject);
begin
  if (setup.ADeps.Count>0) and (setup.ADeps[0]='*getlibs*') then
  begin
   Button1.Enabled:=false;
   DSolveProgress.Visible:=true;
   Label15.Visible:=true;
   Application.ProcessMessages;
   setup.OnMainPosChange:=@DepLoadPosChange;
   setup.OnMaxPosMainChange:=@DepLoadMaxChange;
   setup.ResolveDependencies;
   setup.OnMainPosChange:=@MainPosChange;
   setup.OnMaxPosMainChange:=@MainMaxPosChange;
   Label15.Caption:=strFinished;
   Button1.Enabled:=true;
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

procedure TIWizFrm.InstallationError(Sender: TObject; msg: String);
begin
 ShowMessage(msg);
 InfoMemo.Lines.Add('Installation failed.');
 InfoMemo.Lines.SaveTofile('/tmp/install-'+setup.AppName+'.log');
 setup.Free;
 FreeAndNil(IWizFrm);
 Application.Terminate;
 exit;
end;

procedure TIWizFrm.ITerminateQuestion(Sender: TObject; msg: String);
begin
 if Application.MessageBox(PAnsiChar(msg),PAnsiChar('Listaller question'),MB_YESNO)<>IDYES then
 begin
  ShowMessage(strINClose);
  InfoMemo.Lines.Add('Installation aborted by user.');
  InfoMemo.Lines.SaveTofile('/tmp/install-'+setup.AppName+'.log');
  setup.Free;
  FreeAndNil(IWizFrm);
  Application.Terminate;
 end;
end;

procedure TIWizFrm.FormCreate(Sender: TObject);
var PODirectory, Lang, FallbackLang: String;
    imForm: TimdFrm;
    i: Integer;
begin
if not DirectoryExists(RegDir) then begin
CreateDir(ExtractFilePath(RegDir));
CreateDir(RegDir);
end;

//Load language pack
PODirectory:=GetDataFile('lang/');
GetLanguageIDs(Lang, FallbackLang);
translations.TranslateUnitResourceStrings('LCLStrConsts', PODirectory + 'lclstrconsts-%s.po', Lang, FallbackLang);
translations.TranslateUnitResourceStrings('trstrings', PODirectory + 'listaller-%s.po', Lang, FallbackLang);
writeLn('Language pack loaded.');

if not FileExists(Paramstr(1)) then begin
  ShowMessage(strRunParam);
  halt(1);
  exit;
end;

  DInfo.DName:='';
  Dinfo:=GetDistro;

//Load GTK2 icons
LoadStockPixmap(STOCK_QUIT,ICON_SIZE_BUTTON,FinBtn1.Glyph);

LoadStockPixmap(STOCK_CLOSE,ICON_SIZE_BUTTON,AbortBtn1.Glyph);

LoadStockPixmap(STOCK_GO_FORWARD,ICON_SIZE_BUTTON,Button1.Glyph);

LoadStockPixmap(STOCK_GO_BACK,ICON_SIZE_BUTTON,Button5.Glyph);

//Set translation strings (1)
  Label1.Caption:=strWelcome;
  Label3.Caption:=strnToStart;
  Label4.Caption:=strprogDesc;
  Label5.Caption:=strLicense;
  Label8.Caption:=strPleaseRead;
  Label6.Caption:=strRunning;
  Label7.Caption:=strplWait;
  Label10.Caption:=strComplete;
  Label12.Caption:=strprFinish;
  FinBtn1.Caption:=strFinish;
  AbortBtn1.Caption:=strAbort;
  Button5.Caption:=strBack;
  Button1.Caption:=strNext;
  RadioButton1.Caption:=strIagree;
  RadioButton2.Caption:=strInagree;
  CheckBox1.Caption:=strDispLog;
  Label13.Caption:=strInstallationMode;
  Label14.Caption:=strIModeInstruction;
  ModeGroup.Caption:=strMode;

  NoteBook1.PageIndex:=0;
  NoteBook1.ShowTabs:=false;
  
  if not DirectoryExists(RegDir) then SysUtils.CreateDir(RegDir);
  
  //Check distribution
  if DInfo.DName='' then begin
   ShowMessage(strLDnSupported+#13+strInClose+#13+strNotifyDevs);
   halt;
   exit;
  end;

writeLn('Initialized.');

IWizFrm.Hide;
IWizFrm.Visible:=false;
setup:=TInstallation.Create;
setup.OnError:=@InstallationError;
setup.OnTermQuestion:=@ITerminateQuestion;
//Load the IPK data
setup.Initialize(paramstr(1));
IWizFrm.Show;
IWizFrm.Visible:=true;

//Prepare exectype form

if (setup.pType=lptLinstall)and(not IsRoot) then
begin
  imForm:=TimdFrm.Create(self);
  with imForm do
  begin
    btnTest.Enabled:=true;
    btnHome.Enabled:=true;
    PkWarnImg.Visible:=true;
  if (pos('iotest',setup.Disallows)>0) then
    btnTest.Enabled:=false;
  if (pos('iolocal',setup.Disallows)>0) then
    btnHome.Enabled:=false;
  if (pos('iobase',setup.Disallows)>0) then
    btnInstallAll.Enabled:=false;
  Label13.Caption:=strSpkWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

if (setup.pType=lptDLink)and(not IsRoot) then
begin
  imForm:=TimdFrm.Create(self);
  with imForm do
  begin
  btnTest.Enabled:=false;
  btnHome.Enabled:=false;
  PkWarnImg.Visible:=true;
  Label13.Caption:=strSpkWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

if (setup.pType=lptContainer)and(not IsRoot) then
begin
  imForm:=TimdFrm.Create(self);
  with imForm do
  begin
  btnTest.Enabled:=false;
  pkWarnImg.Visible:=true;
  if (pos('iolocal',setup.Disallows)<=0) then
    btnHome.Enabled:=false;
  if (pos('iobase',setup.Disallows)<=0) then
    btnInstallAll.Enabled:=false;
  Label13.Caption:=strSpkWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

//Check distribution
if (pos(LowerCase(DInfo.DName),setup.Distris)<=0)
and (setup.Distris<>'all') then begin
if Application.MessageBox(PAnsiChar(PAnsiChar(strnSupported)+#13+PAnsiChar(strInstAnyway)),'Distro-Error',MB_YESNO)= IDNO then
 begin
 Application.Terminate;
 exit;
 end;
end;

{ --- Linstallation --- }
if setup.pType=lptLinstall then
begin
//Check if already installed
if not Testmode then
begin
if setup.IsPackageInstalled(setup.AppName,setup.AppID) then
if Application.MessageBox(PAnsiChar(PAnsiChar(strAlreadyInst)+#13+PAnsiChar(strInstallAgain)),PAnsiChar(strReInstall),MB_YESNO)= IDNO then
 begin
  setup.Free;
  Application.Terminate;
  exit;
 end;
end;

//Load all data
if setup.DescFile <> '' then
Memo1.Lines.LoadFromFile(setup.DescFile);
LeftImg.Picture.LoadFromFile(setup.WizImage);
LicMemo.Lines.LoadFromFile(setup.LicenseFile);

Label2.Caption:=StringReplace(strWelcomeTo,'%a',setup.AppName,[rfReplaceAll]);
if Testmode then
begin
IWizFrm.Caption:=StringReplace(strInstOf,'%a',setup.AppName,[rfReplaceAll])+' ['+strTestMode+']';
LblTestMode.Caption:=strTestMode+'!';
LblTestMode.Visible:=true;
end else
IWizFrm.Caption:=StringReplace(strInstOf,'%a',setup.AppName,[rfReplaceAll]);

ListBox1.Items.Add('Distribution: '+DInfo.DName);
ListBox1.Items.Add('Version: '+DInfo.Release);
ListBox1.Items.Add('PackageSystem: '+DInfo.PackageSystem);

Button1.Enabled:=true;
if setup.CMDLn='#' then
CbExecApp.Visible:=false
else CbExecApp.Visible:=true;

//Set profiles to RadioGroup:
for i:=0 to setup.Profiles.Count-1 do
ModeGroup.Items.Add(copy(setup.Profiles[i],0,pos(' #',setup.Profiles[i])));
ModeGroup.ItemIndex:=0;

if (setup.LicenseFile='')and(setup.DescFile='')then
begin
  Button1.Caption:=strInstallNow;
  Button1.Left:=648-Button1.Width;
end;
IWizFrm.Show;
end else
{ --- DLink --- }
if setup.pType=lptDLink then
begin

IWizFrm.Hide;
IWizFrm.Visible:=false;
DGForm:=TDGForm.Create(nil);
DGForm.IIconPath:=setup.AppIcon;
DGForm.IDesktopFiles:=setup.desktopFiles;
with DGForm do
begin
Label1.Caption:=StringReplace(strInstOf,'%a',setup.AppName,[rfReplaceAll]);
Label2.Caption:=strWillDLFiles;
Caption:=Label1.Caption;
Memo1.Lines.LoadFromFile(lp+ExtractFileName(paramstr(1))+'/'+setup.DescFile);
LicMemo.Clear;
LicMemo.Lines.Add(strPkgDownload);

for i:=0 to setup.ADeps.Count-1 do Memo2.Lines.Add(setup.ADeps[i]);
end;
DGForm.Show;
end else
if setup.pType=lptContainer then
begin
IWizFrm.Hide;
IWizFrm.Visible:=false;
setup.Free;
// The setup has already done everything we needed
Application.Terminate;
end;
Application.ShowMainForm:=true;
end;

procedure TIWizFrm.FormDestroy(Sender: TObject);
begin
  //Free instances;
  setup.Free;
  writeLn('Listaller unloaded.');
end;

procedure TIWizFrm.FormShow(Sender: TObject);
begin
{LicRichView:=TRichView.Create(nil);
RVStyle1:=TRVStyle.Create(nil);
LicRichview.Top:=SynEdit1.Top;
LicRichView.Left:=SynEdit1.Left;
LicRichView.Visible:=true;
SynEdit1.Visible:=false;
LicRichView.Parent:=LicPage;
LicRichView.Width:=SynEdit1.Width;
LicRichView.height:=SynEdit1.Height;
LicRichView.Style:=RVStyle1;
LicRichview.lines.LoadFromFile(lp+PkgName+LicenseFile);}
end;

procedure TIWizFrm.GetOutputTimerTimer(Sender: TObject);
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
if Process1.CommandLine<>'' then begin
  repeat
    NoMoreOutput := true;
    Application.ProcessMessages;
    DoStuffForProcess(Process1, InfoMemo);
  until noMoreOutput;
if (Process1.ExitStatus>0) then begin
    GetOutputTimer.Enabled:=false;
    writeLn('Connection to backend broken.');
    writeLn('Cannot resolve dependencies');
    ShowMessage(strCouldntSolve+#13+StringReplace(strViewLog,'%p','/tmp/install-'+setup.AppName+'.log',[rfReplaceAll])+#13+'Code: '+IntToStr(Process1.ExitStatus));
    InfoMemo.Lines.SaveTofile('/tmp/install-'+setup.AppName+'.log');
    halt;
    exit;
  end;
  end;
end;

var HTTP: THTTPSend;FTP: TFTPSend; //<- Has to be global
procedure TIWizFrm.HookSock(Sender: TObject; Reason: THookSocketReason;
const Value: string);
begin
ExProgress.Visible:=true;
Application.ProcessMessages;
//HTTP
if Http.DownloadSize>100 then begin
ExProgress.Max:=HTTP.DownloadSize;
ExProgress.Position:=HTTP.Document.Size;
exit;
end;
//FTP
ExProgress.Position:=FTP.DSock.RecvCounter;
end;

procedure TIWizFrm.MainMaxPosChange(Sender: TObject;max: Integer);
begin
 InsProgress.Max:=max;
 Application.ProcessMessages;
end;

procedure TIWizFrm.ExtraMaxPosChange(Sender: TObject;max: Integer);
begin
 ExProgress.Max:=max;
 Application.ProcessMessages;
end;

procedure TIWizFrm.MainPosChange(Sender: TObject;pos: Integer);
begin
 InsProgress.Position:=pos;
 Application.ProcessMessages;
end;

procedure TIWizFrm.ExtraPosChange(Sender: TObject;pos: Integer);
begin
 ExProgress.Position:=pos;
 Application.ProcessMessages;
end;

procedure TIWizFrm.MainVisibleChange(Sender: TObject;vis: Boolean);
begin
 InsProgress.Visible:=visible;
 Application.ProcessMessages;
end;

procedure TIWizFrm.ExtraVisibleChange(Sender: TObject;vis: Boolean);
begin
 ExProgress.Visible:=visible;
 Application.ProcessMessages;
end;

procedure TIWizFrm.StartInstallation;
var cnf: TIniFile;
begin
while IPage.Visible=false do Application.ProcessMessages;
 Edit1.Visible:=true;
 btn_sendinput.Visible:=true;
 AbortBtn1.Enabled:=false;
 Button1.Enabled:=false;
 Button5.Enabled:=false;
 AbortIns:=false;
 if setup.IFileInfo='' then
 begin
  ShowMessage(strPKGError+#13'Message: No file information that is assigned to this profile was not found!'+#13+strAppClose);
  Application.Terminate;
  exit;
 end;
 AbortBtn1.Enabled:=true;
 ExProgress.Visible:=false;
 Label9.Caption:=strStep1;

 cnf:=TInifile.Create(ConfigDir+'config.cnf');
 //Create HTTP object
  HTTP := THTTPSend.Create;
  HTTP.Sock.OnStatus:=@HookSock;
  HTTP.UserAgent:='Listaller-GET';
 //Create FTP object
  FTP := TFTPSend.Create;
  FTP.DSock.Onstatus:=@HookSock;
 if cnf.ReadBool('Proxy','UseProxy',false) then
 begin
  //Set HTTP
  HTTP.ProxyPort:=cnf.ReadString('Proxy','hPort','');
  HTTP.ProxyHost:=cnf.ReadString('Proxy','hServer','');
  HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
  HTTP.ProxyPass:=cnf.ReadString('Proxy','Password',''); //The PW is visible in the file! It should be crypted

 //Not needed
 {if DInfo.Desktop='GNOME' then begin
 HTTP.ProxyPass:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_user');
 HTTP.ProxyUser:=CmdResult('gconftool-2 -g /system/http_proxy/authentication_password');
  end;
 //Set FTP
 FTP.:=cnf.ReadString('Proxy','fPort','');
 HTTP.ProxyHost:=cnf.ReadString('Proxy','fServer','');
 HTTP.ProxyUser:=cnf.ReadString('Proxy','Username','');
 HTTP.ProxyPass:=cnf.ReadString('Proxy','Password','');  }

 end;
  cnf.Free;
 //Assign HTTP/FTP objects to Installation service object
 setup.HTTPSend:=HTTP;
 setup.FTPSend:=FTP;

 //Assign event handlers
 setup.OnMainPosChange:=@MainPosChange;
 setup.OnExtraPosChange:=@ExtraPosChange;
 setup.OnMaxPosMainChange:=@MainMaxPosChange;
 setup.OnMaxPosExtraChange:=@ExtraMaxPosChange;
 setup.OnMainVisibleChange:=@MainVisibleChange;
 setup.OnExtraVisibleChange:=@ExtraVisibleChange;
 setup.OnStateMessage:=@setupStateMessage;

 setup.CurrentProfile:=ModeGroup.Items[ModeGroup.ItemIndex];

 GetOutPutTimer.Enabled:=true;
 setup.DoInstallation(Process1,InfoMemo.Lines);
 GetOutPutTimer.Enabled:=false;
 setup.HTTPSend:=nil;
 setup.FTPSend:=nil;
 HTTP.Free;
 FTP.Free;

 if not Testmode then
begin
NoteBook1.PageIndex:=5;

Label11.Caption:=StringReplace(strWasInstalled,'%a',setup.AppName,[rfReplaceAll]);
FinBtn1.Visible:=true;
AbortBtn1.Visible:=false;
FinPage.Refresh;
end else
begin
  Process1.CommandLine:=setup.CMDLn;
  Process1.Options:=[poWaitOnExit];
  Hide;
  Application.ProcessMessages;
  Process1.Execute;
  ShowMessage(strTestFinished);
   Process1.CommandLine := 'rm -rf /tmp/litest';
   Process1.Execute;
  Application.Terminate;
 end;

end;

procedure TIWizFrm.btn_sendinputClick(Sender: TObject);
var InputString: String;
begin
if Process1.Running then begin
  InputString:=Edit1.text;
  Edit1.SelectAll;
  Process1.Input.Write(InputString[1], length(InputString));
  end else ShowMessage('Error - Process not running!');
end;

procedure TIWizFrm.RadioButton1Change(Sender: TObject);
begin
  with Sender as TRadioButton do begin
  if Checked then Button1.Enabled:=true
  else Button1.Enabled:=false;
  end;
end;

procedure TIWizFrm.setupStateMessage(Sender: TObject; msg: String);
begin
 Label9.Caption:=msg;
end;

initialization
  {$I igobase.lrs}

end.

