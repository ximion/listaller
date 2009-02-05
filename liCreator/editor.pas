{ editor.pas
  Copyright (C) Listaller Project 2008-2009

  editor.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  editor.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}
//** IPS source graphical editor
unit editor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SynEdit,
  ComCtrls, Menus, StdCtrls, fwiz, FileUtil, SynHighlighterXML, ExtCtrls,
  process, SynHighlighterTeX, SynHighlighterAny, MD5;

type

  { TfrmEditor }

  TfrmEditor = class(TForm)
    mmMain: TMainMenu;
    MainScriptEdit: TSynEdit;
    FilesEdit: TSynEdit;
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
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PageControl1: TPageControl;
    Process1: TProcess;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    SynTeXSyn1: TSynTeXSyn;
    SynXMLSyn1: TSynXMLSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure mnuBuildCreatePackageClick(Sender: TObject);
    procedure mnuEditAddFilePathClick(Sender: TObject);
    procedure mnuEditUndoClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuFileNewWizardClick(Sender: TObject);
    procedure mnuFileNewBlankClick(Sender: TObject);
    procedure mnuBuildFastClick(Sender: TObject);
    procedure mnuFileSaveAsClick(Sender: TObject);
    procedure mnuFileLoadIPSClick(Sender: TObject);
    procedure mnuFileCloseClick(Sender: TObject);
    procedure mnuEditFileWizardClick(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
  private
    { private declarations }
 //   procedure CreateMD5Sums;
    procedure SaveIPSFile(IFn: String);
    procedure ReadOutput;
  public
    { public declarations }
    FileInfo: TStringList;
  end; 

const
   //** Size of the Linux output pipe
   READ_BYTES = 2048;

var
  frmEditor: TfrmEditor;
  //** Name of the current file
  FName: String;

implementation

uses prjwizard;

{ TfrmEditor }

procedure TfrmEditor.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  if Assigned(FileInfo) then FileInfo.Free;
end;

procedure TfrmEditor.FormShow(Sender: TObject);
begin

end;

procedure TfrmEditor.IdleTimer1Timer(Sender: TObject);
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
    DoStuffForProcess(Process1, memLog);
  until noMoreOutput;
  sleep(800);
  PageControl1.Enabled:=true;
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
  
procedure TfrmEditor.mnuBuildCreatePackageClick(Sender: TObject);
begin
  mnuFileSaveClick(nil);
  if SaveDialog1.Execute then
  begin
    if FileExists(SaveDialog1.FileName) then
    begin
      ShowMessage('This file already exists.'#13'Please choose another one!');
      exit;
    end;
    Application.ProcessMessages;
    SetFocus;

    Process1.CommandLine:='lipa -b '+''''+FName+''' '''+SaveDialog1.FileName+'''';
    memLog.Lines.Add('Execute '+Process1.CommandLine+' ...');
    PageControl1.Enabled:=false;
    Process1.Execute;
    ReadOutput();
    if Process1.ExitStatus>0 then ShowMessage('Build failed!');
  end;
end;

procedure TfrmEditor.mnuEditAddFilePathClick(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    if TabSheet1.Visible then
      MainScriptEdit.Lines.Add(OpenDialog2.FileName);
    if TabSheet2.Visible then
      FilesEdit.Lines.Add(OpenDialog2.FileName);
  end;
end;

procedure TfrmEditor.mnuEditUndoClick(Sender: TObject);
begin
  if TabSheet1.Visible then
    MainScriptEdit.Undo;
  if TabSheet2.Visible then
    FilesEdit.Undo;
end;

procedure TfrmEditor.SaveIPSFile(IFn: String);
var ips,ipsx: TStringList;i: Integer;
begin
  ips:=TStringList.Create;
  ipsx:=TStringList.Create;

  I:=0;

  ips.Clear;
  ips.Add('IPK-Source.Version: 0.8');
  for i:=0 to MainScriptEdit.Lines.Count-1 do
    ips.Add(MainScriptEdit.Lines[i]);

  //Preprocess files
  if Assigned(FileInfo) then
  begin
    ips.Add('!-Files #0');           // id=0 -> Standard-Installation
    //NEEDS IMPROVEMENTS!!
    for i:=0 to FilesEdit.Lines.Count-1 do
    if i mod 2 = 0 then
    begin
      ips.Add('>'+FilesEdit.Lines[i]);
      ips.add(FilesEdit.Lines[i+1]);
      ips.Add(MD5.MDPrint(MD5.MD5File(FilesEdit.Lines[i+1],1024)));
    end;
  end;
  for i:=0 to ipsx.Count-1 do
    ips.Add(ipsx[i]);
  ipsx.free;
  ips.SaveToFile(IFn);
  ips.Free;
  FName:=Ifn;
end;

procedure TfrmEditor.mnuFileSaveClick(Sender: TObject);
begin
  if FName<>'' then
  begin
    SaveIPSFile(FName);
  end else
  begin
    mnuFileSaveAsClick(Sender);
  end;
end;

procedure TfrmEditor.mnuFileNewWizardClick(Sender: TObject);
begin
  frmProjectWizard:=TfrmProjectWizard.Create(nil);
  frmProjectWizard.ShowModal;
  frmProjectWizard.Free;
end;

procedure TfrmEditor.mnuFileNewBlankClick(Sender: TObject);
begin
  FName:='';
  MainScriptEdit.Lines.Clear;
  FilesEdit.Lines.Clear;
end;

procedure TfrmEditor.ReadOutput;
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
     end;
   until n <= 0;
   M.Free;
end;

procedure TfrmEditor.mnuBuildFastClick(Sender: TObject);
var M: TMemoryStream;
   n: LongInt;
   BytesRead: LongInt;
   s: String;
begin
  mnuFileSaveClick(nil);
  Application.ProcessMessages;
  SetFocus;

  Process1.CommandLine:='lipa -b '+''''+FName+'''';
  memLog.Lines.Add('Execute '+Process1.CommandLine+' ...');
  PageControl1.Enabled:=false;
  Process1.Execute;
   ReadOutput();
  if Process1.ExitStatus>0 then ShowMessage('Build failed!');
end;

procedure TfrmEditor.Button1Click(Sender: TObject);
begin

end;

var fActiv: Boolean=true;
procedure TfrmEditor.FormActivate(Sender: TObject);
begin
  if fActiv then
  begin
    fActiv:=false;
    frmProjectWizard.ShowModal;
    frmProjectWizard.Free;
  end;
end;

{
OutOfDate:
procedure TfrmEditor.CreateMD5Sums;
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

procedure TfrmEditor.mnuFileSaveAsClick(Sender: TObject);
begin
  if SaveDialog2.Execute then
  begin
    if SaveDialog2.FileName<>'' then
      SaveIPSFile(SaveDialog2.FileName);
  end;
end;

procedure TfrmEditor.mnuFileLoadIPSClick(Sender: TObject);

  function BeginsFilesPart(str: String):Boolean;
  begin
    Result := False;
    if (Length(str)>=7) then
    begin
     if (Copy(str,1,7)='!-Files') then
     begin
       Result := True;
     end;
    end;
  end;

var ips: TStringList;i,j: integer;
begin
  if OpenDialog1.Execute then
  if FileExists(OpenDialog1.FileName) then
  begin
    ips:=TStringList.Create;
    ips.LoadFromFile(OpenDialog1.FileName);
    MainScriptEdit.Lines.Clear;
    FilesEdit.Lines.Clear;
    for i:=1 to ips.Count-1 do
    begin
      if BeginsFilesPart(ips[i]) then break
      else MainScriptEdit.Lines.Add(ips[i]);
    end;
    if i<>ips.Count-1 then FileInfo:=TStringList.Create;
    for j:=i+1 to ips.Count-1 do            // change counting method
      if ((j-(i+1)) mod 3 = 0) then         // counting method needs imrovement !!!
      begin
        FilesEdit.Lines.Add(ips[j]);
        FilesEdit.Lines.Add(ips[j+1]);
        FileInfo.Add(ips[j+2]);
      end;

    FName:=OpenDialog1.FileName;
    Caption:='Listaller package creator - "'+ExtractFileName(FName)+'"';
 end;
end;

procedure TfrmEditor.mnuFileCloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmEditor.mnuEditFileWizardClick(Sender: TObject);
begin
  frmFileWizard.ShowModal;
end;

procedure TfrmEditor.TabSheet1Show(Sender: TObject);
begin
  mnuEditFileWizard.Visible:=false;
end;

procedure TfrmEditor.TabSheet2Show(Sender: TObject);
begin
  mnuEditFileWizard.Visible:=true;
end;

procedure TfrmEditor.TabSheet3Show(Sender: TObject);
begin
  // StatusBar1.Panels[0].Text:='Loading MD5-sums...';
  Application.ProcessMessages;
  // StatusBar1.Panels[0].Text:='Ready';
  Application.ProcessMessages;
end;

initialization
  {$I editor.lrs}

end.

