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
//** This unit contains the code for the graphical installation of standard IPK-packages
unit igobase;

{$mode objfpc}{$H+}

interface

uses
  Forms, distri, Buttons, Classes, Dialogs,
  LCLIntf, LCLType, LiTypes, LiUtils, Process, xTypeFm, ComCtrls, Controls, ExtCtrls,
  FileUtil, Graphics, StdCtrls, SysUtils, LiInstaller, StrLocale, IconLoader, LResources;

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
    Label15: TLabel;
    Label17: TLabel;
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
    FailPage: TPage;
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
    //** True if installation is beeing aborted
    AbortIns: Boolean;
    //** Start the GUI inatallation
    procedure StartInstallation;
  public
    SetupFailed: Boolean;
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

{$R igobase.lfm}

uses dgfrm;

{ TIWizFrm }

procedure TIWizFrm.Button1Click(Sender: TObject);
begin
  case Notebook1.PageIndex of
    5: exit;
    4: exit;
    3:
    begin
      setup.SetProfileID(ModeGroup.ItemIndex);
      Button5.Visible := false;
      Button1.Visible := false;
      NoteBook1.PageIndex := 4;
      StartInstallation;
      exit;
    end;
    2:
    begin
      if setup.GetFileList <> '*' then
      begin
        Button5.Visible := false;
        Button1.Visible := false;
        NoteBook1.PageIndex := 4;
        StartInstallation;
        exit;
      end;
      Button1.Caption := rsInstallNow;
      NoteBook1.PageIndex := 3;
    end;
    1:
    begin
      if (LicMemo.Lines.Count <= 0) and (setup.GetFileList <> '*') then
      begin
        Button5.Visible := false;
        Button1.Visible := false;
        NoteBook1.PageIndex := 3;
        StartInstallation;
        exit;
      end;

      if setup.getFileList <> '*' then
      begin
        Button1.Caption := rsInstallNow;
      end;

      if RadioButton2.Checked then
        Button1.Enabled := false
      else
        Button1.Enabled := true;


      if (LicMemo.Lines.Count <= 0) then
      begin
        Button1.Caption := rsInstallNow;
        Notebook1.PageIndex := 3;
      end
      else
        Notebook1.PageIndex := 2;
    end;
    0:
    begin
      if (DescMemo.Lines.Count <= 0) and (LicMemo.Lines.Count <= 0) and
        (setup.GetFileList <> '*') then
      begin
        Button5.Visible := false;
        Button1.Visible := false;
        NoteBook1.PageIndex := 3;
        StartInstallation;
        exit;
      end;
      if (DescMemo.Lines.Count <= 0) then
      begin
        Button1.Caption := rsInstallNow;
        if RadioButton2.Enabled then
          Button1.Enabled := false;
        NoteBook1.PageIndex := 2;
        Button5.Visible := true;
        exit;
      end;
      if (LicMemo.Lines.Count > 0) and (LicMemo.Lines.Count <= 0) then
      begin
        NoteBook1.PageIndex := 1;
        Button5.Visible := true;
        Button1.Caption := rsInstallNow;
        exit;
      end;
      NoteBook1.PageIndex := 1;
      Button5.Visible := true;
    end;
  end;
end;

procedure TIWizFrm.AbortBtn1Click(Sender: TObject);
begin
  if NoteBook1.PageIndex = 5 then
  begin
    Label10.Caption := rsInstAborted;
    Label11.Caption := StrSubst(rsAppNInstall, '%a', setup.GetAppName);
    AbortIns := true;
  end
  else
    Application.Terminate;
end;

procedure TIWizFrm.Button5Click(Sender: TObject);
begin
  case NoteBook1.PageIndex of
    1:
    begin
      Button1.Caption := rsNext;
      Notebook1.PageIndex := 0;
      Button5.Visible := false;
      Button1.Enabled := true;
    end;
    2:
    begin
      Button1.Caption := rsNext;

      if DescMemo.Lines.Count <= 0 then
        Notebook1.PageIndex := 0
      else
        Notebook1.PageIndex := 1;
      Button1.Enabled := true;
    end;
    3:
    begin
      Button1.Caption := rsNext;

      if LicMemo.Lines.Count <= 0 then
        Notebook1.PageIndex := 1
      else
        Notebook1.PageIndex := 2;

      Button1.Enabled := true;
    end;
    4: exit;
  end;
end;

procedure TIWizFrm.CheckBox1Change(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
  begin
    ListBox1.Visible := true;
    Infomemo.Visible := true;
  end
  else
  begin
    ListBox1.Visible := false;
    Infomemo.Visible := false;
  end;
end;

procedure TIWizFrm.FinBtn1Click(Sender: TObject);
begin
  FinBtn1.Enabled := false;
  Label16.Caption := rsCleaningUp;
  Label16.Visible := true;
  FinPage.Refresh;

  if not SetupFailed then
    if (setup.GetAppCMD <> '#') and (CbExecApp.Checked) then
    begin
      Process1.CommandLine := setup.GetAppCMD;
      Process1.Execute;
    end;

  Application.Terminate;
end;

function IsCommandRunning(cmd: String): Boolean;
var
  t: TProcess;
  s: TStringList;
begin
  Result := false;
  t := TProcess.Create(nil);
  t.CommandLine := FindBinary('ps') + ' -A' + cmd;
  t.Options := [poUsePipes, poWaitonexit];
  try
    t.Execute;
    s := TStringList.Create;
    try
      s.LoadFromStream(t.Output);
      Result := Pos(cmd, s.Text) > 0;
    finally
      s.Free;
    end;
  finally
    t.Free;
  end;
end;

function RequestHandling(mtype: LiRqType; msg: PChar; udata: Pointer): LiRqResult; cdecl;
var
  s: String;
begin
  //Window title
  if setup.GetAppName = '' then
    s := rsInstallation
  else
    s := Format(rsInstOf, [setup.GetAppName]);
  //Handle interaction types
  case mtype of
    rqError:
    begin
      Application.MessageBox(msg, 'Error', MB_OK + MB_IconError);
      if Assigned(IWizFrm) then
      begin
        with IWizFrm do
        begin
          InfoMemo.Lines.Add(rsInstFailed);
          InfoMemo.Lines.SaveTofile('/tmp/install-' + setup.GetAppName + '.log');
          NoteBook1.PageIndex := 6;
          Label17.Caption := StrSubst(rsCouldNotInstallApp, '%a', setup.GetAppName);
          SetupFailed := true;

          FinBtn1.Visible := true;
          AbortBtn1.Visible := false;
        end;
      end
      else
      begin
        setup.Free;
        Result := rqsOK;
        halt(6); //Kill application
        exit;
      end;
    end;
    rqWarning:
    begin
      if Application.MessageBox(PAnsiChar(msg), PAnsiChar(s), MB_YESNO +
        MB_IconWarning) <> idYes then
      begin
        ShowMessage(rsINClose);
        Result := rqsNo;
        if Assigned(IWizFrm) then
        begin
          with IWizFrm do
          begin
            InfoMemo.Lines.Add(rsInstAbortedByUser);
            InfoMemo.Lines.SaveTofile('/tmp/install-' + setup.GetAppName + '.log');
            NoteBook1.PageIndex := 6;
            Label17.Caption := StrSubst(rsCouldNotInstallApp, '%a', setup.GetAppName);
            SetupFailed := true;

            FinBtn1.Visible := true;
            AbortBtn1.Visible := false;
          end;
        end
        else
        begin
          setup.Free;
          Application.Terminate;
        end;
      end;
    end;
    rqQuestion:
    begin
      if Application.MessageBox(PAnsiChar(msg), PAnsiChar(s), MB_YESNO +
        MB_IconQuestion) <> idYes then
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

procedure StatusChangeCall(change: LiStatusChange; data: LiStatusData;
  user_data: Pointer); cdecl;
begin
  if Assigned(IWizFrm) then
    with IWizFrm do
    begin
      case change of
        scMnProgress: InsProgress.Position := Data.mnprogress;
        scExProgress:
        begin
          ExProgress.Position := Data.exprogress;
          if (Data.exprogress = 0) and (ExProgress.Visible = true) then
            ExProgress.Visible := false
          else
            ExProgress.Visible := true;
        end;
        scMessage:
        begin //Necessary to add messages to log, even if it is invisible (to generate report)
          InfoMemo.Lines.Add(Data.msg);
          pinfo(Data.msg);
        end;
        scStepMessage: Label9.Caption := Data.msg;
      end;
    end

  else
    if Assigned(DGForm) then
      with DGForm do
      begin
        case change of
          scMnProgress: MainProgress.Position := Data.mnprogress;
          scExProgress:
          begin
            DlProgress.Position := Data.exprogress;
            if (Data.exprogress = 0) and (DlProgress.Visible = true) then
              DlProgress.Visible := false
            else
              DlProgress.Visible := true;
          end;
          scMessage:
          begin //Necessary to add messages to log, even if it is invisible (to generate report)
            Memo3.Lines.Add(Data.msg);
            pinfo(Data.msg);
          end;
          scStepMessage: ;//Label9.Caption:=data.msg;
        end;
      end
    else
    begin
      if setup.PkType <> ptContainer then
      begin
        pwarning('Listaller Setup tool seems to owns none of the required display forms!');
        pwarning('This should _never_ happen!');
      end;
    end;

  Application.ProcessMessages;
end;

function LoadIPKFile(): Boolean;
var
  imForm: TimdFrm;
  sig: PkgSignatureState;
  forcedOptions: String;
  testmd: Boolean;
begin
  testmd := false;
  if not DirectoryExists(RegDir) then
  begin
    CreateDir(ExtractFilePath(RegDir));
    CreateDir(RegDir);
  end;

  if not FileExists(ParamStr(1)) then
  begin
    ShowMessage(rsRunParam);
    halt(1);
    Result := false;
    Application.Terminate;
  end;

  Application.ProcessMessages;

  DInfo.DName := '';
  Dinfo := GetDistro;

  Result := true;
  //Check distribution
  if DInfo.DName = '' then
  begin
    ShowMessage(rsLDnSupported + #10 + rsInClose + #10 + rsNotifyDevs);
    Result := false;
    Application.Terminate;
    exit;
  end;

  writeLn('Begin loading IPK');
  imForm := TIMdFrm.Create(nil);
  imForm.EnterLoadingState;

  setup := TInstallPack.Create;
  setup.SetUserRequestCall(@RequestHandling, nil);
  //Set status handler for init stage
  setup.SetStatusChangeEvent(@xtypefm.PkgInitProgressChange, imForm);

  //Set forced actions
  if Application.HasOption('force-architecture') then
    forcedOptions := 'architecture;';
  if Application.HasOption('force-depends') then
    forcedOptions := 'dependencies;';
  setup.Forced := forcedOptions;

  //Load the IPK data
  setup.Initialize(ParamStr(1));

  //Now set new status change callback handler
  setup.SetStatusChangeEvent(@StatusChangeCall, nil);

  sig := setup.GetSignatureState;

  //Prepare exectype form
  if (setup.PkType = ptLinstall) and (not Superuser) then
  begin
    imForm.LeaveLoadingState;
    with imForm do
    begin
      SetSigState(sig);
      btnTest.Enabled := true;
      btnHome.Enabled := true;
      PkWarnImg.Visible := true;
      if (pos('iotest', setup.GetDisallows) > 0) then
        btnTest.Enabled := false;
      if (pos('iolocal', setup.GetDisallows) > 0) then
        btnHome.Enabled := false;
      if (pos('iobase', setup.GetDisallows) > 0) then
        btnInstallAll.Enabled := false;
      PkILabel.Caption := rsSpkWarning;

      ShowModal;
    end;
    testmd := imForm.IsTestMode;
    imForm.Free;
  end;

  if (setup.PkType = ptDLink) and (not Superuser) then
  begin
    imForm.LeaveLoadingState;
    with imForm do
    begin
      SetSigState(sig);
      btnTest.Enabled := false;
      btnHome.Enabled := false;
      PkWarnImg.Visible := true;
      PkILabel.Caption := rsSpkWarning;

      ShowModal;
    end;
    testmd := imForm.IsTestMode;
    imForm.Free;
  end;

  if (setup.PkType = ptContainer) and (not Superuser) then
  begin
    imForm.LeaveLoadingState;
    with imForm do
    begin
      SetSigState(sig);
      btnTest.Visible := false;
      pkWarnImg.Visible := true;
      //Do not parse disallows for containerIPK at time
      //??? Switch this on later!
      //if (pos('iolocal', setup.GetDisallows) > 0) then
      //  btnHome.Enabled := false;
      //if (pos('iobase', setup.GetDisallows) > 0) then
      btnInstallAll.Enabled := false;
      PkILabel.Caption := rsSpkWarning;

      ShowModal;
    end;
    testmd := imForm.IsTestMode;
    imForm.Free;
  end;

{//Check distribution
if (pos(LowerCase(DInfo.DName),setup.GetSupDistris)<=0)
and (setup.GetSupDistris<>'all') then
begin
if Application.MessageBox(PAnsiChar(PAnsiChar(rsnSupported)+#13+PAnsiChar(rsInstAnyway)),'Distro-Error',MB_YESNO)= IDNO then
 begin
 Result:=false;
 Application.Terminate;
 exit;
 end;
end;}

  //Set if Testmode
  setup.Testmode := testmd;

  writeLn('Superuser: ', Superuser);
  //Set if root installation
  setup.SetRootMode(Superuser);

end;

procedure TIWizFrm.FormCreate(Sender: TObject);
var
  i: Integer;
  tmp: TStringList;
begin

  //Load GTK2 icons
  LoadStockPixmap(STOCK_QUIT, ICON_SIZE_BUTTON, FinBtn1.Glyph);
  LoadStockPixmap(STOCK_CLOSE, ICON_SIZE_BUTTON, AbortBtn1.Glyph);
  LoadStockPixmap(STOCK_GO_FORWARD, ICON_SIZE_BUTTON, Button1.Glyph);
  LoadStockPixmap(STOCK_GO_BACK, ICON_SIZE_BUTTON, Button5.Glyph);

  //Set translation strings (1)
  Label1.Caption := rsWelcome;
  Label3.Caption := rsnToStart;
  Label4.Caption := rsprogDesc;
  Label5.Caption := rsLicense;
  Label8.Caption := rsPleaseRead;
  Label6.Caption := rsRunning;
  Label7.Caption := rsplWait;
  Label10.Caption := rsComplete;
  Label12.Caption := rsprFinish;
  FinBtn1.Caption := rsFinish;
  AbortBtn1.Caption := rsAbort;
  Button5.Caption := rsBack;
  Button1.Caption := rsNext;
  RadioButton1.Caption := rsIagree;
  RadioButton2.Caption := rsInagree;
  CheckBox1.Caption := rsDispLog;
  Label13.Caption := rsInstallationMode;
  Label14.Caption := rsIModeInstruction;
  ModeGroup.Caption := rsMode;
  CbExecApp.Caption := rsExecNewApp;

  NoteBook1.PageIndex := 0;
  NoteBook1.ShowTabs := false;

  if not DirectoryExists(RegDir) then
    SysUtils.CreateDir(RegDir);

  //Load all setup data

  { --- Linstallation --- }

  DescMemo.Lines.Clear;
  setup.ReadLongDescription(TStringList(DescMemo.Lines));


  LeftImg.Picture.LoadFromFile(setup.GetWizardImagePath);
  LicMemo.Lines.Clear;
  setup.ReadLicense(TStringList(LicMemo.Lines));

  Label2.Caption := StrSubst(rsWelcomeTo, '%a', setup.GetAppName);
  if setup.Testmode then
  begin
    IWizFrm.Caption := StrSubst(rsInstOf, '%a', setup.GetAppName) +
      ' [' + rsTestMode + ']';
    LblTestMode.Caption := rsTestMode + '!';
    LblTestMode.Visible := true;
  end
  else
    IWizFrm.Caption := StrSubst(rsInstOf, '%a', setup.GetAppName);

  ListBox1.Items.Add('Distribution: ' + DInfo.DName);
  ListBox1.Items.Add('Version: ' + DInfo.Release);
  ListBox1.Items.Add('PackageSystem: ' + DInfo.PackageSystem);

  Button1.Enabled := true;

  if setup.GetAppCMD = '#' then
    CbExecApp.Visible := false
  else
    CbExecApp.Visible := true;

  //Set profiles to RadioGroup:
  setup.ReadProfiles(TStringList(ModeGroup.Items));
  ModeGroup.ItemIndex := 0;

  if (LicMemo.Lines.Count <= 0) and (DescMemo.Lines.Count <= 0) then
  begin
    Button1.Caption := rsInstallNow;
    Button1.Left := 648 - Button1.Width;
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
  if Assigned(setup) then
    setup.Free;
  pdebug('Listaller unloaded.');
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
  NoMoreOutput: Boolean;

  procedure DoStuffForProcess(Process: TProcess; OutputMemo: TMemo);
  var
    Buffer: String;
    BytesAvailable: DWord;
    BytesRead: longint;
  begin

    if Process.Running then
    begin

      BytesAvailable := Process.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable > 0 do
      begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := Process.OutPut.Read(Buffer[1], BytesAvailable);
        OutputMemo.Text := OutputMemo.Text + copy(Buffer, 1, BytesRead);
        writeLn(OutputMemo.Text + copy(Buffer, 1, BytesRead));
        Application.ProcessMessages;
        BytesAvailable := Process.OutPut.NumBytesAvailable;
        NoMoreOutput := false;
      end;
      if BytesRead > 0 then
        OutputMemo.SelStart := Length(OutputMemo.Text);
    end;

  end;

begin
  if Process1.CommandLine <> '' then
  begin
    repeat
      NoMoreOutput := true;
      Application.ProcessMessages;
      DoStuffForProcess(Process1, InfoMemo);
    until noMoreOutput;
    if (Process1.ExitStatus > 0) then
    begin
      GetOutputTimer.Enabled := false;
      writeLn('Connection to backend broken.');
      writeLn(rsCannotResolv);
      ShowMessage(rsCouldntSolve + #13 + StrSubst(rsViewLog,
        '%p', '/tmp/install-' + setup.GetAppName + '.log') + #13 +  'Code: ' +
        IntToStr(Process1.ExitStatus));
      InfoMemo.Lines.SaveTofile('/tmp/install-' + setup.GetAppName + '.log');
      halt;
      exit;
    end;
  end;
end;

procedure TIWizFrm.StartInstallation;
begin
  while IPage.Visible = false do
    Application.ProcessMessages;
  AbortBtn1.Enabled := false;
  Button1.Enabled := false;
  Button5.Enabled := false;
  AbortIns := false;

  if setup.GetFileList = '' then
  begin
    ShowMessage(rsPKGError + #10'Message: No file information was found for this profile!'
      +  #10 + rsAppClose);
    Application.Terminate;
    exit;
  end;

  AbortBtn1.Enabled := true;
  ExProgress.Visible := false;
  Label9.Caption := rsStep1;

  setup.SetProfileID(ModeGroup.ItemIndex);

  setup.StartInstallation;

  if not SetupFailed then
    if not setup.Testmode then
    begin
      NoteBook1.PageIndex := 5;

      Label11.Caption := StrSubst(rsWasInstalled, '%a', setup.GetAppName);

      FinBtn1.Visible := true;
      AbortBtn1.Visible := false;
      FinPage.Refresh;
    end
    else
    begin
      pdebug('Testmode: Executing new app...');
      Process1.CommandLine := setup.GetAppCMD;
      Process1.Options := [poWaitOnExit, poUsePipes];
      Hide;
      Application.ProcessMessages;
      if FileExists(Process1.CommandLine) then
        Process1.Execute
      else
        ShowMessage(rsCNFindAppExecutable);
      ShowMessage(rsTestFinished);
      Process1.CommandLine := 'rm -rf /tmp/litest';
      Process1.Execute;
      Application.Terminate;
    end;

end;

procedure TIWizFrm.RadioButton1Change(Sender: TObject);
begin
  with Sender as TRadioButton do
  begin
    if Checked then
      Button1.Enabled := true
    else
      Button1.Enabled := false;
  end;
end;

end.

