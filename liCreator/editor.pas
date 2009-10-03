{ Copyright (C) 2008-2009 Matthias Klumpp

  Authors:
   Matthias Klumpp
   Thomas Dieffenbach

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, version 3.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program. If not, see <http://www.gnu.org/licenses/>.}
//** IPS source GUI editor & generator
unit editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SynEdit,
  ComCtrls, Menus, StdCtrls, fwiz, FileUtil, SynHighlighterXML, ExtCtrls,
  process, SynHighlighterTeX, SynHighlighterAny, SynEditTypes, MD5;

type

  { TFileProfile }

  TFileProfile=class
    FPage: TPage;
    FSynEdit: TSynEdit;
    FProfileIndex: Integer;
  public
    constructor Create(AOwner: TObject; ParentNotebook: TNotebook; ProfileIndex: Integer);
    destructor Destroy; override;
  published
    property ProfileIndex: Integer read FProfileIndex write FProfileIndex;
    property Page: TPage read FPage;
    property SynEdit: TSynEdit read FSynEdit;
  end;

  { TFileProfiles }

  TFileProfiles=class
    FFileProfiles: TList;
    FParentNotebook: TNotebook;
  private
    function GetNewProfileIndex: Integer;
    function GetCount: Integer;
  public
    constructor Create(AOwner: TObject; ParentNotebook: TNotebook);
    destructor Destroy; override;
    function AddProfile: TFileProfile; overload;
    function AddProfile(ProfileIndex: Integer): TFileProfile; overload;
    procedure RemoveProfile(APage: TPage); overload;
    procedure RemoveProfile(ProfileIndex: Integer); overload;
    procedure Remove(Index: Integer);
    function Profiles_By_Page(APage: TPage): TFileProfile;
    function Profiles_By_ProfileIndex(ProfileIndex: Integer): TFileProfile;
    function Profiles_By_Index(Index: Integer): TFileProfile;
    procedure Clear;
  published
    property Count: Integer read GetCount;
  end;

  { TFrmEditor }

  TFrmEditor = class(TForm)
    FindDialog: TFindDialog;
    MainScriptEdit: TSynEdit;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnuEditReplace: TMenuItem;
    mnuEditRemoveFileProfile: TMenuItem;
    mnuEditAddFileProfile: TMenuItem;
    mmMain: TMainMenu;
    memLog: TMemo;
    mnuFile: TMenuItem;
    mnuEditPaste: TMenuItem;
    mnuBuild: TMenuItem;
    mnuBuildCreatePackage: TMenuItem;
    mnuEditAddFilePath: TMenuItem;
    mnuEditUndo: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileNewWizard: TMenuItem;
    mnuFileNewBlank: TMenuItem;
    mnuBuildFast: TMenuItem;
    mnuEdit: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuFileLoadIPS: TMenuItem;
    mnuFileClose: TMenuItem;
    mnuEditFileWizard: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditFind: TMenuItem;
    IPSNotebook: TNotebook;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    ReplaceDialog: TReplaceDialog;
    ScriptPage: TPage;
    Process1: TProcess;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Splitter1: TSplitter;
    IPSHighlight: TSynAnySyn;
    FileHighlight: TSynAnySyn;
    procedure Button1Click(Sender: TObject);
    procedure FindDialogFind(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure IPSNotebookCloseTabClicked(Sender: TObject);
    procedure mnuBuildCreatePackageClick(Sender: TObject);
    procedure mnuEditAddFilePathClick(Sender: TObject);
    procedure mnuEditAddFileProfileClick(Sender: TObject);
    procedure mnuEditCopyClick(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditFindClick(Sender: TObject);
    procedure mnuEditPasteClick(Sender: TObject);
    procedure mnuEditRemoveFileProfileClick(Sender: TObject);
    procedure mnuEditReplaceClick(Sender: TObject);
    procedure mnuEditUndoClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuFileNewWizardClick(Sender: TObject);
    procedure mnuFileNewBlankClick(Sender: TObject);
    procedure mnuBuildFastClick(Sender: TObject);
    procedure mnuFileSaveAsClick(Sender: TObject);
    procedure mnuFileLoadIPSClick(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure mnuEditFileWizardClick(Sender: TObject);
    procedure ReplaceDialogFind(Sender: TObject);
    procedure ReplaceDialogReplace(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
  private
    { private declarations }
 //   procedure CreateMD5Sums;
  //** Saves the IPS file
    procedure SaveIPSFile(IFn: String);
  //** Reads the Output of Process1
    procedure ReadOutput;
    function GetActiveSynEdit: TSynEdit;
  public
  { public declarations }
    procedure NewBlank;
  end; 

const
   //** Size of the Linux output pipe
   READ_BYTES = 2048;
   FORM_CAPTION = 'Listaller package creator';

var
  FrmEditor: TFrmEditor;
  //** Name of the current file
  FName: String;
  //** Container for file profiles
  FileProfiles: TFileProfiles;

implementation

uses prjwizard;

{TFileProfile}

constructor TFileProfile.Create(AOwner: TObject; ParentNotebook: TNotebook; ProfileIndex: Integer);
var
  temp: Integer;
begin
  inherited Create;
  FProfileIndex := ProfileIndex;
  temp := ParentNotebook.Pages.Add('File-Profile #' + IntToStr(FProfileIndex));
  FPage := ParentNotebook.Page[temp];
  FSynEdit := TSynEdit.Create(FPage);
  FSynEdit.Parent := FPage;
  FSynEdit.Align := alClient;

  FSynEdit.Gutter.LeftOffset:=0;
  FSynEdit.Gutter.RightOffset:=0;
  FSynEdit.Font.Size := 10;
  FSynEdit.Highlighter := FrmEditor.FileHighlight;
end;

destructor TFileProfile.Destroy();
begin
  FreeAndNil(FSynEdit);
  FreeAndNil(FPage);
  inherited Destroy;
end;

{TFileProfiles}

constructor TFileProfiles.Create(AOwner: TObject; ParentNotebook: TNotebook);
begin
  inherited Create;
  FParentNotebook := ParentNotebook;
  FFileProfiles := TList.Create;
end;

destructor TFileProfiles.Destroy();
var
  k: Integer;
begin
  for k:=FFileProfiles.Count-1 downto 0 do
  begin
    TFileProfile(FFileProfiles[k]).Free;
  end;
  FFileProfiles.Free;
  inherited Destroy;
end;

function TFileProfiles.GetNewProfileIndex: Integer;
var
  k,ind: Integer;
  used: Boolean;
begin
  ind := -1;
  repeat
    inc(ind);
    used := false;
    for k:=0 to FFileProfiles.Count-1 do
      if TFileProfile(FFileProfiles[k]).ProfileIndex=ind then
        used := true;
  until not used;
  Result := ind;
end;

function TFileProfiles.GetCount : Integer;
begin
  Result := FFileProfiles.Count;
end;

function TFileProfiles.AddProfile: TFileProfile;
var
  temp, iIndex: Integer;
begin
  temp := GetNewProfileIndex;
  iIndex := FFileProfiles.Add(TFileProfile.Create(Self, FParentNotebook, temp));
  TFileProfile(FFileProfiles[iIndex]).ProfileIndex:=temp;
  Result := TFileProfile(FFileProfiles[iIndex]);
end;

function TFileProfiles.AddProfile(ProfileIndex: Integer): TFileProfile;
var
  iIndex: Integer;
begin
  iIndex := FFileProfiles.Add(TFileProfile.Create(Self, FParentNotebook, ProfileIndex));
  TFileProfile(FFileProfiles[iIndex]).ProfileIndex:=ProfileIndex;
  Result := TFileProfile(FFileProfiles[iIndex]);
end;

procedure TFileProfiles.Clear;
var
  i: Integer;
begin
  for i:=FFileProfiles.Count-1 downto 0 do
  begin
    Remove(i);
  end;
end;

procedure TFileProfiles.RemoveProfile(APage: TPage);
var
  k: Integer;
begin
  for k:=FFileProfiles.Count-1 downto 0 do
  begin
    if TFileProfile(FFileProfiles[k]).Page =APage then
    begin
      Remove(k);
      break;
    end;
  end;
end;

procedure TFileProfiles.RemoveProfile(ProfileIndex: Integer);
var
  k: Integer;
begin
  for k:=FFileProfiles.Count-1 downto 0 do
  begin
    if TFileProfile(FFileProfiles[k]).ProfileIndex=ProfileIndex then
    begin
      Remove(k);
      break;
    end;
  end;
end;

procedure TFileProfiles.Remove(Index: Integer);
begin
  TFileProfile(FFileProfiles[Index]).Free;
  FFileProfiles.Delete(Index);
end;

// ---------- Returns FileProfile by active Notebook-Page  -------------
function TFileProfiles.Profiles_By_Page(APage: TPage): TFileProfile;
var
  k: Integer;
begin
  Result := nil;
  for k:=FFileProfiles.Count-1 downto 0 do
  begin
    if TFileProfile(FFileProfiles[k]).Page =APage then
    begin
      Result := TFileProfile(FFileProfiles[k]);
      exit;
    end;
  end;
end;

// ---------- Returns FileProfile by ProfileIndex "!-Files #X" ----------
function TFileProfiles.Profiles_By_ProfileIndex(ProfileIndex: Integer): TFileProfile;
var
  k: Integer;
begin
  Result := nil;
  for k:=FFileProfiles.Count-1 downto 0 do
  begin
    if TFileProfile(FFileProfiles[k]).ProfileIndex=ProfileIndex then
    begin
      Result := TFileProfile(FFileProfiles[k]);
      exit;
    end;
  end;
end;

// ---------- Returns FileProfile by Index in TList(FileProfiles) ----------
function TFileProfiles.Profiles_By_Index(Index: Integer): TFileProfile;
begin
  Result := nil;
  if not (Index>FFileProfiles.Count-1) then
    Result := TFileProfile(FFileProfiles[Index]);
end;

{ TFrmEditor }

procedure TFrmEditor.FormCreate(Sender: TObject);
begin
  IPSNotebook.PageIndex:=0;
  FileProfiles := TFileProfiles.Create(Self, IPSNotebook);
end;

procedure TFrmEditor.FormShow(Sender: TObject);
begin

end;

procedure TFrmEditor.IdleTimer1Timer(Sender: TObject);
(*  var
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
      ShowMessage(Inttostr(bytesavailable));
      while BytesAvailable>0 do
       begin
        ShowMEssage(Inttostr(bytesavailable));
        SetLength(Buffer, BytesAvailable);
        BytesRead := Process.OutPut.Read(Buffer[1], BytesAvailable);
        OutputMemo.Text := OutputMemo.Text + copy(Buffer,1, BytesRead);
        //OutputMemo.Lines.Add(copy(Buffer,1, BytesRead));
        Application.ProcessMessages;
        BytesAvailable := Process.OutPut.NumBytesAvailable;
        NoMoreOutput := false;
      end;
      NoMoreOutput := true;
      if BytesRead>0 then
        OutputMemo.SelStart := Length(OutputMemo.Text);
    end;
  end;  *)
begin
 (* repeat
    NoMoreOutput := true;
    Application.ProcessMessages;
    DoStuffForProcess(Process1, memLog);
  until noMoreOutput;
  sleep(800);
  IPSNotebook.Enabled:=true;*)
end;

procedure TFrmEditor.IPSNotebookCloseTabClicked(Sender: TObject);
begin
  if Sender is TPage then
    FileProfiles.RemoveProfile(TPage(Sender));
end;


function FileCopy(source,dest: String): Boolean;
var
 fSrc,fDst,len: Integer;
 ct,units,size: Longint;
 buffer: packed array [0..2047] of Byte;
begin
 ct:=0;
 Result := False; { Assume that it WONT work }
 if source <> dest then begin
   fSrc := FileOpen(source,fmOpenRead);
   if fSrc >= 0 then begin
     size := FileSeek(fSrc,0,2);
     units:=size div 2048;
     FileSeek(fSrc,0,0);
     fDst := FileCreate(dest);
     if fDst >= 0 then begin
       while size > 0 do begin
         len := FileRead(fSrc,buffer,sizeof(buffer));
         FileWrite(fDst,buffer,len);
         size := size - len;
         if units > 0 then
         ct:=ct+1;
       end;
       FileSetDate(fDst,FileGetDate(fSrc));
       FileClose(fDst);
       FileSetAttr(dest,FileGetAttr(source));
       Result := True;
     end;
     FileClose(fSrc);
   end;
 end;
end;
  
procedure TFrmEditor.mnuBuildCreatePackageClick(Sender: TObject);
begin
  mnuFileSaveClick(nil);
  if SaveDialog1.Execute then
  begin
    if FileExists(SaveDialog1.FileName) then
    begin
      if not (MessageDlg('This file already exists.'#13'Overwrite it?',mtCOnfirmation,
        [mbYes,mbNo],0)=mrYes) then
      exit;
      DeleteFile(SaveDialog1.FileName);
    end;
    Application.ProcessMessages;
    SetFocus;

    Process1.CommandLine:='lipa -b '+''''+FName+''' '''+SaveDialog1.FileName+'''';
    memLog.Lines.Add('Execute '+Process1.CommandLine+' ...');
    IPSNotebook.Enabled:=false;
    Process1.Execute;
    ReadOutput();
    if Process1.ExitStatus>0 then ShowMessage('Build failed!');
    IPSNotebook.Enabled:=true;
  end;
end;

procedure TFrmEditor.mnuEditAddFilePathClick(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    if (ScriptPage.Visible) and (IPSNotebook.ActivePageComponent=ScriptPage) then
      MainScriptEdit.Lines.Add(OpenDialog2.FileName)
    else if FileProfiles.Profiles_By_Page(IPSNotebook.ActivePageComponent)<>nil then
       FileProfiles.Profiles_By_Page(IPSNotebook.ActivePageComponent).SynEdit.Lines.Add(OpenDialog2.FileName);
  end;
end;

procedure TFrmEditor.mnuEditAddFileProfileClick(Sender: TObject);
begin
  FileProfiles.AddProfile();
end;

procedure TFrmEditor.mnuEditCopyClick(Sender: TObject);
begin
  if FrmEditor.ActiveControl is TSynEdit then
    TSynEdit(FrmEditor.ActiveControl).CopyToClipboard;
end;

procedure TFrmEditor.mnuEditCutClick(Sender: TObject);
begin
  if FrmEditor.ActiveControl is TSynEdit then
    TSynEdit(FrmEditor.ActiveControl).CutToClipboard;
end;

procedure TFrmEditor.mnuEditFindClick(Sender: TObject);
begin
  FindDialog.Execute;
end;

procedure TFrmEditor.mnuEditPasteClick(Sender: TObject);
begin
  if FrmEditor.ActiveControl is TSynEdit then
    TSynEdit(FrmEditor.ActiveControl).PasteFromClipboard;
end;

procedure TFrmEditor.mnuEditRemoveFileProfileClick(Sender: TObject);
begin
  FileProfiles.RemoveProfile(IPSNotebook.Page[IPSNotebook.PageIndex]);
end;

procedure TFrmEditor.mnuEditReplaceClick(Sender: TObject);
begin
  ReplaceDialog.Execute;
end;

procedure TFrmEditor.mnuEditUndoClick(Sender: TObject);
begin
  //if ScriptPage.Visible then
  //  MainScriptEdit.Undo;
  //if Page2.Visible then
  //  FilesEdit.Undo;
  if FrmEditor.ActiveControl is TSynEdit then
    TSynEdit(FrmEditor.ActiveControl).Undo;
end;

procedure TFrmEditor.SaveIPSFile(IFn: String);
var ips,ipsx: TStringList;i,k: Integer;
begin
  ips:=TStringList.Create;
  ipsx:=TStringList.Create;

  I:=0;

  ips.Clear;
  for i:=0 to MainScriptEdit.Lines.Count-1 do
    ips.Add(MainScriptEdit.Lines[i]);

  //Preprocess files
  ips.Add('');
  for k:=0 to FileProfiles.Count-1 do
  begin
    ips.Add('!-Files ~'+IntToStr(FileProfiles.Profiles_By_Index(k).ProfileIndex));
    //NEEDS IMPROVEMENTS!!
    for i:=0 to FileProfiles.Profiles_By_Index(k).SynEdit.Lines.Count-1 do
      if i mod 2 = 0 then
      begin
        ips.Add(FileProfiles.Profiles_By_Index(k).SynEdit.Lines[i]);
        ips.Add(FileProfiles.Profiles_By_Index(k).SynEdit.Lines[i+1]);
       // ips.Add(MD5.MDPrint(MD5.MD5File(FileProfiles.Profiles_By_Index(k).SynEdit.Lines[i+1],1024)));
      end;
  end;

  for i:=0 to ipsx.Count-1 do
    ips.Add(ipsx[i]);
  ipsx.free;
  ips.SaveToFile(IFn);
  ips.Free;
  FName:=Ifn;
end;

procedure TFrmEditor.mnuFileSaveClick(Sender: TObject);
begin
  if FName<>'' then
  begin
    SaveIPSFile(FName);
  end else
  begin
    mnuFileSaveAsClick(Sender);
  end;
end;

procedure TFrmEditor.mnuFileNewWizardClick(Sender: TObject);
begin
  NewBlank;
  frmProjectWizard:=TfrmProjectWizard.Create(nil);
  frmProjectWizard.ShowModal;
  frmProjectWizard.Free;
end;

procedure TFrmEditor.mnuFileNewBlankClick(Sender: TObject);
begin
  NewBlank;
end;

procedure TFrmEditor.NewBlank;
begin
  FName:='';
  Caption:= FORM_CAPTION;
  MainScriptEdit.Lines.Clear;
  FileProfiles.Clear;
end;

procedure TFrmEditor.ReadOutput;
var M: TMemoryStream;
   n: LongInt;
   BytesRead: LongInt;
   s: String;
begin
 M := TMemoryStream.Create;
 m.Clear;
   BytesRead := 0;
   while Process1.Running do
   begin
     M.SetSize(BytesRead + READ_BYTES);
     n := Process1.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if n > 0
     then begin
       //Convert to string and write
       SetString(s, PChar(M.Memory + BytesRead), n);
       memLog.Lines.Add(s);
       Inc(BytesRead, n);
       Application.ProcessMessages;
       // the following line is necessary for the Memo to scroll down, bug in LCL?
       memLog.Lines.Delete(memLog.Lines.Add(''));
     end
     else begin
       // no data, wait 100 ms
       Sleep(100);
     end;
   end;
   // read last part
   repeat
     M.SetSize(BytesRead + READ_BYTES);
     // try reading it
     n := Process1.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
     if n > 0
     then begin
       SetString(s, PChar(M.Memory + BytesRead), n);
       memLog.Lines.Add(s);
       Inc(BytesRead, n);
       Application.ProcessMessages;
       // the following line is necessary for the Memo to scroll down, bug in LCL?
       memLog.Lines.Delete(memLog.Lines.Add(''));
     end;
   until n <= 0;
   M.Free;
end;

procedure TFrmEditor.mnuBuildFastClick(Sender: TObject);
var strTargetName: String;
begin
  mnuFileSaveClick(nil);
  if not FileExists(FName) then exit;
  Application.ProcessMessages;
  SetFocus;
  strTargetName := ChangeFileExt(FName,'.ipk');
  if FileExists(strTargetName) then DeleteFile(strTargetName);
  Process1.CommandLine:='lipa -b '+''''+FName+''' '''+strTargetName+'''';
  memLog.Lines.Add('Execute '+Process1.CommandLine+' ...');
  IPSNotebook.Enabled:=false;
  Process1.Execute;
  ReadOutput();
  if Process1.ExitStatus>0 then ShowMessage('Build failed!');
  IPSNotebook.Enabled:=true;
end;

procedure TFrmEditor.Button1Click(Sender: TObject);
begin

end;

function TFrmEditor.GetActiveSynEdit: TSynEdit;
begin
  try
    if IPSNotebook.ActivePageComponent=ScriptPage then
      Result := MainScriptEdit
    else
      Result := FileProfiles.Profiles_By_Page(IPSNotebook.ActivePageComponent).SynEdit;
  except
    Result := nil;
  end;
end;

procedure TFrmEditor.FindDialogFind(Sender: TObject);
var
  AEdit: TSynEdit;
  srOptions: TSynSearchOptions;
begin
  AEdit := GetActiveSynEdit;
  if AEdit= nil then
    exit;
  srOptions := [];
  if not (frDown in FindDialog.Options) then Include(srOptions,ssoBackwards);
  if (frMatchCase in FindDialog.Options) then Include(srOptions, ssoMatchCase);
  if (frWholeWord in FindDialog.Options) then Include(srOptions, ssoWholeWord);
  if AEdit.SearchReplace(FindDialog.FindText,'',srOptions)=0 then
  begin
    ShowMessage('Not found.');
  end;
end;

var fActiv: Boolean=true;
procedure TFrmEditor.FormActivate(Sender: TObject);
begin
  if fActiv then
  begin
    fActiv:=false;
    frmProjectWizard.ShowModal;
    frmProjectWizard.Free;
  end;
end;

procedure TFrmEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(FileProfiles);
end;

{
OutOfDate:
procedure TFrmEditor.CreateMD5Sums;
var i: Integer;
begin
if FilesEdit.Lines.Count mod 2 = 0 then begin

for i:=0 to FilesEdit.Lines.Count-1 do begin;
if FileExists(FilesEdit.Lines[i*2]) then
MD5View.Lines.Add(MD5.MDPrint(MD5.MD5File(DeleteModifiers(FilesEdit.Lines[i*2]),1024)));
 end;

end;
end;
}



procedure TFrmEditor.mnuFileSaveAsClick(Sender: TObject);
begin
  if SaveDialog2.Execute then
  begin
    if SaveDialog2.FileName<>'' then
      SaveIPSFile(SaveDialog2.FileName);
  end;
end;

procedure TFrmEditor.mnuFileLoadIPSClick(Sender: TObject);

  function BeginsFilesPart(str: String;var iProfileIndex:Integer):Boolean;
  var
    k: Integer;
  begin
    Result := False;
    if (Length(str)>=7) then
    begin
     if LowerCase(Copy(str,1,7))='!-files' then
     begin
       Result := True;
       try
         k:=Pos('#',str);
         iProfileIndex := StrToInt(Copy(str,k+1,Length(str)-k));
       except
         iProfileIndex := -1;
       end;
     end;
    end;
  end;

  function IsInstallationPath(str: String):Boolean;
  begin
    Result := False;
    if length(str)>0 then Result := str[1]='>';
  end;

  function IsFilePath(str: String):Boolean;
  begin
    Result := False;
    if length(str)>0 then Result := str[1]='/';
  end;

var
  ips: TStringList;
  i,j,iProfileIndex: integer;
  AFileEdit: TSynEdit;
  strInstallPath: String;
begin
  if OpenDialog1.Execute then
  if FileExists(OpenDialog1.FileName) then
  begin
    NewBlank;
    ips:=TStringList.Create;
    ips.LoadFromFile(OpenDialog1.FileName);
    for i:=1 to ips.Count-1 do
    begin
      if BeginsFilesPart(ips[i],iProfileIndex) then break
      else MainScriptEdit.Lines.Add(ips[i]);
    end;

    j:=i;
    AFileEdit := nil;
    while (j<=ips.count-2) do
    begin
      if BeginsFilesPart(ips[j],iProfileIndex) then
      begin
        AFileEdit := FileProfiles.AddProfile(iProfileIndex).SynEdit;
      end else if IsInstallationPath(ips[j]) then
      begin
        strInstallPath := ips[j];
      end else if IsFilePath(ips[j]) then
      begin
       if not (AFileEdit=nil) then
       begin
         AFileEdit.Lines.Add(strInstallPath);
         AFileEdit.Lines.Add(ips[j]);
        end;
      end;
      inc(j);
    end;

    FName:=OpenDialog1.FileName;
    Caption:= FORM_CAPTION+ ' - "'+ExtractFileName(FName)+'"';
 end;
end;

procedure TFrmEditor.mnuFileCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TFrmEditor.mnuEditFileWizardClick(Sender: TObject);
begin
  frmFileWizard.ShowModal;
end;

procedure TFrmEditor.ReplaceDialogFind(Sender: TObject);
var
  AEdit: TSynEdit;
  srOptions: TSynSearchOptions;
begin
  AEdit := GetActiveSynEdit;
  if AEdit= nil then
    exit;
  srOptions := [];
  if not (frDown in ReplaceDialog.Options) then Include(srOptions,ssoBackwards);
  if (frMatchCase in ReplaceDialog.Options) then Include(srOptions, ssoMatchCase);
  if (frWholeWord in ReplaceDialog.Options) then Include(srOptions, ssoWholeWord);
  if AEdit.SearchReplace(ReplaceDialog.FindText,ReplaceDialog.ReplaceText,srOptions)=0 then
  begin
    ShowMessage('Not found.');
  end;
end;

procedure TFrmEditor.ReplaceDialogReplace(Sender: TObject);
var
  AEdit: TSynEdit;
  srOptions: TSynSearchOptions;
begin
  AEdit := GetActiveSynEdit;
  if AEdit= nil then
    exit;
  srOptions := [ssoReplace];
  if not (frDown in ReplaceDialog.Options) then Include(srOptions,ssoBackwards);
  if (frMatchCase in ReplaceDialog.Options) then Include(srOptions, ssoMatchCase);
  if (frWholeWord in ReplaceDialog.Options) then Include(srOptions, ssoWholeWord);
  if (frReplaceAll in ReplaceDialog.Options) then Include(srOptions, ssoReplaceAll);

  if (AEdit.SelAvail) and (AEdit.SelStart>0) and (not(ssoBackwards in srOptions)) then
    // Set SelStart one position back, otherwise the next element will be replaced
    AEdit.SelStart := AEdit.SelStart-1;
  if (AEdit.SelAvail) and (AEdit.SelStart>0) and (ssoBackwards in srOptions) then
    // Set SelStart to SelEnd, otherwise the previous element will be replaced
    AEdit.SelStart := AEdit.SelEnd;
  if AEdit.SearchReplace(ReplaceDialog.FindText,ReplaceDialog.ReplaceText,srOptions)=0 then
  begin
    ShowMessage('Not found.');
  end;
end;

procedure TFrmEditor.TabSheet1Show(Sender: TObject);
begin
  mnuEditFileWizard.Visible:=false;
end;

procedure TFrmEditor.TabSheet2Show(Sender: TObject);
begin
  mnuEditFileWizard.Visible:=true;
end;

procedure TFrmEditor.TabSheet3Show(Sender: TObject);
begin
  // StatusBar1.Panels[0].Text:='Loading MD5-sums...';
  Application.ProcessMessages;
  // StatusBar1.Panels[0].Text:='Ready';
  Application.ProcessMessages;
end;

initialization
  {$I editor.lrs}

end.

