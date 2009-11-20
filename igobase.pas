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
//** This unit contains the code for the graphical installation of standard IPK-packages
unit igobase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, FileUtil, ExtCtrls, process, Buttons, LCLType,
  LCLIntf, distri, LiBasic, trStrings, SynEdit, xTypeFm, Installer,
  liTypes, IconLoader;

type

  { TIWizFrm }

  //** The installer wizard window
  TIWizFrm = class(TForm)
    AbortBtn1: TBitBtn;
    Button1: TBitBtn;
    Button5: TBitBtn;
    CheckBox1: TCheckBox;
    CbExecApp: TCheckBox;
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
    DescMemo: TMemo;
    LicMemo: TMemo;
    Notebook1: TNotebook;
    OpenDialog1: TOpenDialog;
    IMPage: TPage;
    ModeGroup: TRadioGroup;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetOutputTimerTimer(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
  private
    { private declarations }
    //** True if installation is beeing aborted
    AbortIns: Boolean;
    //** Start the GUI inatallation
    procedure StartInstallation;
  public
    { public declarations }
  end; 

var
  IWizFrm: TIWizFrm;
  //** Distribution information
  DInfo: TDistroInfo;
  //** IPK installation object
  setup: TInstallPack;

  //** Load the IPK file
  function LoadIPKFile(): Boolean;

implementation

uses DGUnit;

{ TIWizFrm }

procedure TIWizFrm.Button1Click(Sender: TObject);
begin
case Notebook1.PageIndex of
5: exit;
4: exit;
3: begin
    setup.SetProfileID(ModeGroup.ItemIndex);
    Button5.Visible:=false;
    Button1.Visible:=false;
    NoteBook1.PageIndex:=4;
    StartInstallation;
    exit;
  end;
2: begin
        if setup.GetFileList<>'*' then
        begin
           Button5.Visible:=false;
           Button1.Visible:=false;
           NoteBook1.PageIndex:=4;
           StartInstallation;
           exit;
        end;
        Button1.Caption:=rsInstallNow;
        NoteBook1.PageIndex:=3;
   end;
1: begin
 if (LicMemo.Lines.Count<=0)and(setup.GetFileList<>'*') then
 begin
  Button5.Visible:=false;
  Button1.Visible:=false;
  NoteBook1.PageIndex:=3;
  StartInstallation;
  exit;
 end;

     if setup.getFileList<>'*' then
     begin
        Button1.Caption:=rsInstallNow;
     end;

  if RadioButton2.Checked then
  Button1.Enabled:=false
  else
  Button1.Enabled:=true;


   if (LicMemo.Lines.Count<=0) then
  begin
    Button1.Caption:=rsInstallNow;
    Notebook1.PageIndex:=3 end else
    Notebook1.PageIndex:=2;
  end;
0: begin
 if (DescMemo.Lines.Count<=0)and(LicMemo.Lines.Count<=0)and(setup.GetFileList<>'*') then
 begin
  Button5.Visible:=false;
  Button1.Visible:=false;
  NoteBook1.PageIndex:=3;
  StartInstallation;
  exit;
 end;
 if (DescMemo.Lines.Count<=0) then begin
  Button1.Caption:=rsInstallNow;
 if RadioButton2.Enabled then
  Button1.Enabled:=false;
  NoteBook1.PageIndex:=2;
  Button5.Visible:=true;
  exit;
 end;
 if (LicMemo.Lines.Count>0)and(LicMemo.Lines.Count<=0) then begin
  NoteBook1.PageIndex:=1;
  Button5.Visible:=true;
  Button1.Caption:=rsInstallNow;
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
  Label10.Caption:=rsInstAborted;
  Label11.Caption:=StringReplace(rsAppNInstall,'%a',setup.GetAppName,[rfReplaceAll]);
  AbortIns:=true;
end else
  Application.Terminate;
end;

procedure TIWizFrm.Button5Click(Sender: TObject);
begin
case NoteBook1.PageIndex of
1: begin
  Button1.Caption:=rsNext;
  Notebook1.PageIndex:=0;
  Button5.Visible:=false;
  Button1.Enabled:=true;
  end;
2: begin
  Button1.Caption:=rsNext;
 { Button1.Width:=83;
  Button1.Left:=566; }

  if DescMemo.Lines.Count<=0 then
  Notebook1.PageIndex:=0
  else
  Notebook1.PageIndex:=1;
  Button1.Enabled:=true;
  end;
3: begin
        Button1.Caption:=rsNext;

        if LicMemo.Lines.Count<=0 then
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
  Label16.Caption:=rsCleaningUp;
  Label16.Visible:=true;
  FinPage.Refresh;
  DeleteDirectory(lp+ExtractFileName(paramstr(1)),false);

  if (setup.GetAppCMD<>'#')and(CbExecApp.Checked) then
  begin
   Process1.CommandLine:=setup.GetAppCMD;
   Process1.Execute;
  end;

  Application.Terminate;
end;

procedure MainPosChange(pos: LongInt;data: Pointer);cdecl;
begin
 IWizFrm.InsProgress.Position:=pos;
 Application.ProcessMessages;
end;

procedure ExtraPosChange(pos: LongInt;data: Pointer); cdecl;
begin
with IWizFrm do
begin
 ExProgress.Position:=pos;
 if(pos=0)and(ExProgress.Visible=true)then ExProgress.Visible:=false
 else ExProgress.Visible:=true;

 Application.ProcessMessages;
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

function RequestHandling(mtype: TRqType;msg: PChar;data: Pointer): TRqResult; cdecl;
begin
with IWizFrm do
begin
case mtype of
rqError: begin
  ShowMessage(msg);
  Result:=rqsOK;
  if Assigned(InfoMemo) then
  begin
   InfoMemo.Lines.Add(rsInstFailed);
   InfoMemo.Lines.SaveTofile('/tmp/install-'+setup.GetAppName+'.log');
  end;
  setup.Free;
  Application.Terminate;
  exit;
end;
rqWarning: begin
  if Application.MessageBox(PAnsiChar(msg),PAnsiChar(Format(rsInstOf,[setup.GetAppName])),MB_YESNO)<>IDYES then
  begin
   ShowMessage(rsINClose);
   Result:=rqsNo;
   InfoMemo.Lines.Add('Installation aborted by user.');
   InfoMemo.Lines.SaveTofile('/tmp/install-'+setup.GetAppName+'.log');
   setup.Free;
   Application.Terminate;
   FreeAndNil(IWizFrm);
  end;
end;
rqQuestion: begin
  if Application.MessageBox(PAnsiChar(msg),PAnsiChar(Format(rsInstOf,[setup.GetAppName])),MB_YESNO)<>IDYES then
   Result:=rqsNo else Result:=rqsYes;
end;
rqInfo: begin
  ShowMessage(msg);
  Result:=rqsOK;
end;
 end;
end;

end;

procedure MessageCall(msg: PChar;imp: TMType;data: Pointer);cdecl;
begin
 writeLn(msg);
 if Assigned(IWizFrm) then
 IWizFrm.InfoMemo.Lines.Add(msg); //Needed for Log-messages, even if the control is invisible
 Application.ProcessMessages;
end;

procedure StepMessage(msg: PChar;imp: TMType;data: Pointer);cdecl;
begin
 IWizFrm.Label9.Caption:=msg;
end;

function LoadIPKFile(): Boolean;
var imForm: TimdFrm;
begin
if not DirectoryExists(RegDir) then
begin
CreateDir(ExtractFilePath(RegDir));
CreateDir(RegDir);
end;

if not FileExists(Paramstr(1)) then
begin
  ShowMessage(rsRunParam);
  halt(1);
  Result:=false;
  Application.Terminate;
end;

  DInfo.DName:='';
  Dinfo:=GetDistro;

 Result:=true;
 //Check distribution
  if DInfo.DName='' then begin
   ShowMessage(rsLDnSupported+#13+rsInClose+#13+rsNotifyDevs);
   Result:=false;
   Application.Terminate;
   exit;
  end;

writeLn('Begin loading IPK');

setup:=TInstallPack.Create;
setup.SetUserRequestCall(@RequestHandling,nil);
setup.SetMessageCall(@MessageCall,nil);

//Set forced actions
if Application.HasOption('force-architecture') then
 setup.Forced:='architecture;';
//Load the IPK data
setup.Initialize(paramstr(1));

//Prepare exectype form

if (setup.PkType=ptLinstall)and(not Superuser) then
begin
  imForm:=TimdFrm.Create(nil);
  with imForm do
  begin
    btnTest.Enabled:=true;
    btnHome.Enabled:=true;
    PkWarnImg.Visible:=true;
  if (pos('iotest',setup.GetDisallows)>0) then
    btnTest.Enabled:=false;
  if (pos('iolocal',setup.GetDisallows)>0) then
    btnHome.Enabled:=false;
  if (pos('iobase',setup.GetDisallows)>0) then
    btnInstallAll.Enabled:=false;
  PkILabel.Caption:=rsSpkWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

if (setup.PkType=ptDLink)and(not Superuser) then
begin
  imForm:=TimdFrm.Create(nil);
  with imForm do
  begin
  btnTest.Enabled:=false;
  btnHome.Enabled:=false;
  PkWarnImg.Visible:=true;
  PkILabel.Caption:=rsSpkWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

if (setup.PkType=ptContainer)and(not Superuser) then
begin
  imForm:=TimdFrm.Create(nil);
  with imForm do
  begin
  btnTest.Visible:=false;
  pkWarnImg.Visible:=true;
  if (pos('iolocal',setup.GetDisallows)>0) then
    btnHome.Enabled:=false;
  if (pos('iobase',setup.GetDisallows)>0) then
    btnInstallAll.Enabled:=false;
  PkILabel.Caption:=rsSpkWarning;
  //
  ShowModal;
  end;
  imForm.Free;
end;

//Check distribution
if (pos(LowerCase(DInfo.DName),setup.GetSupDistris)<=0)
and (setup.GetSupDistris<>'all') then begin
if Application.MessageBox(PAnsiChar(PAnsiChar(rsnSupported)+#13+PAnsiChar(rsInstAnyway)),'Distro-Error',MB_YESNO)= IDNO then
 begin
 Result:=false;
 Application.Terminate;
 exit;
 end;
end;

 //Set if Testmode
 setup.SetTestmode(Testmode);
 //Set if root installation
 setup.SetRootMode(Superuser);
end;

procedure TIWizFrm.FormCreate(Sender: TObject);
var i: Integer;
    tmp: TStringList;
begin

//Load GTK2 icons
LoadStockPixmap(STOCK_QUIT,ICON_SIZE_BUTTON,FinBtn1.Glyph);
LoadStockPixmap(STOCK_CLOSE,ICON_SIZE_BUTTON,AbortBtn1.Glyph);
LoadStockPixmap(STOCK_GO_FORWARD,ICON_SIZE_BUTTON,Button1.Glyph);
LoadStockPixmap(STOCK_GO_BACK,ICON_SIZE_BUTTON,Button5.Glyph);

//Set translation strings (1)
  Label1.Caption:=rsWelcome;
  Label3.Caption:=rsnToStart;
  Label4.Caption:=rsprogDesc;
  Label5.Caption:=rsLicense;
  Label8.Caption:=rsPleaseRead;
  Label6.Caption:=rsRunning;
  Label7.Caption:=rsplWait;
  Label10.Caption:=rsComplete;
  Label12.Caption:=rsprFinish;
  FinBtn1.Caption:=rsFinish;
  AbortBtn1.Caption:=rsAbort;
  Button5.Caption:=rsBack;
  Button1.Caption:=rsNext;
  RadioButton1.Caption:=rsIagree;
  RadioButton2.Caption:=rsInagree;
  CheckBox1.Caption:=rsDispLog;
  Label13.Caption:=rsInstallationMode;
  Label14.Caption:=rsIModeInstruction;
  ModeGroup.Caption:=rsMode;

  NoteBook1.PageIndex:=0;
  NoteBook1.ShowTabs:=false;
  
  if not DirectoryExists(RegDir) then SysUtils.CreateDir(RegDir);

//Load all setup data

{ --- Linstallation --- }

DescMemo.Lines.Clear;
setup.ReadLongDescription(TStringList(DescMemo.Lines));


LeftImg.Picture.LoadFromFile(setup.GetWizardImagePath);
LicMemo.Lines.Clear;
setup.ReadLicense(TStringList(LicMemo.Lines));

Label2.Caption:=StringReplace(rsWelcomeTo,'%a',setup.GetAppName,[rfReplaceAll]);
if Testmode then
begin
IWizFrm.Caption:=StringReplace(rsInstOf,'%a',setup.GetAppName,[rfReplaceAll])+' ['+rsTestMode+']';
LblTestMode.Caption:=rsTestMode+'!';
LblTestMode.Visible:=true;
end else
IWizFrm.Caption:=StringReplace(rsInstOf,'%a',setup.GetAppName,[rfReplaceAll]);

ListBox1.Items.Add('Distribution: '+DInfo.DName);
ListBox1.Items.Add('Version: '+DInfo.Release);
ListBox1.Items.Add('PackageSystem: '+DInfo.PackageSystem);

Button1.Enabled:=true;

if setup.GetAppCMD='#' then
CbExecApp.Visible:=false
else CbExecApp.Visible:=true;

//Set profiles to RadioGroup:
setup.ReadProfiles(TStringList(ModeGroup.Items));
ModeGroup.ItemIndex:=0;

if (LicMemo.Lines.Count<=0)and(DescMemo.Lines.Count<=0)then
begin
  Button1.Caption:=rsInstallNow;
  Button1.Left:=648-Button1.Width;
end;

try
if FileExists(setup.GetAppIcon) then
begin
 IWizFrm.Icon.LoadFromFile(setup.GetAppIcon);
 Application.Icon.LoadFromFile(setup.GetAppIcon);
end;
except
end;

IWizFrm.Show;
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
    writeLn(rsCannotResolv);
    ShowMessage(rsCouldntSolve+#13+StringReplace(rsViewLog,'%p','/tmp/install-'+setup.GetAppName+'.log',[rfReplaceAll])+#13+'Code: '+IntToStr(Process1.ExitStatus));
    InfoMemo.Lines.SaveTofile('/tmp/install-'+setup.GetAppName+'.log');
    halt;
    exit;
  end;
  end;
end;

procedure TIWizFrm.StartInstallation;
begin
while IPage.Visible=false do Application.ProcessMessages;
 AbortBtn1.Enabled:=false;
 Button1.Enabled:=false;
 Button5.Enabled:=false;
 AbortIns:=false;

 if setup.GetFileList='' then
 begin
  ShowMessage(rsPKGError+#13'Message: No file information was found for this profile!'+#13+rsAppClose);
  Application.Terminate;
  exit;
 end;

 AbortBtn1.Enabled:=true;
 ExProgress.Visible:=false;
 Label9.Caption:=rsStep1;

 //Assign event handlers
 setup.SetMainChangeCall(@MainPosChange,nil);
 setup.SetExtraChangeCall(@ExtraPosChange,nil);

 setup.SetStepMessageCall(@StepMessage,nil);

 setup.SetProfileID(ModeGroup.ItemIndex);

 setup.StartInstallation;

 if not Testmode then
begin
NoteBook1.PageIndex:=5;

Label11.Caption:=StringReplace(rsWasInstalled,'%a',setup.GetAppName,[rfReplaceAll]);

FinBtn1.Visible:=true;
AbortBtn1.Visible:=false;
FinPage.Refresh;
end else
begin
  Process1.CommandLine:=setup.GetAppCMD;
  Process1.Options:=[poWaitOnExit];
  Hide;
  Application.ProcessMessages;
  Process1.Execute;
  ShowMessage(rsTestFinished);
   Process1.CommandLine := 'rm -rf /tmp/litest';
   Process1.Execute;
  Application.Terminate;
 end;

end;

procedure TIWizFrm.RadioButton1Change(Sender: TObject);
begin
  with Sender as TRadioButton do begin
  if Checked then Button1.Enabled:=true
  else Button1.Enabled:=false;
  end;
end;

initialization
  {$I igobase.lrs}

end.

