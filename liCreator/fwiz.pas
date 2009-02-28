{ fwiz.pas
  Copyright (C) Listaller Project 2008

  fwiz.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  fwiz.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}
//** Wizard that helps adding new files to an IPS script
unit fwiz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, Buttons, Fileutil, SynEdit;

type

  { TfrmFileWizard }

  TfrmFileWizard = class(TForm)
    btnAddFiles: TBitBtn;
    btnAbort: TBitBtn;
    edtFolder: TDirectoryEdit;
    edtFolderCopyToPath: TEdit;
    edtFileCopyToPath: TEdit;
    edtFIle: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    rbAddFilesFromFolders: TRadioButton;
    rbAddSingleFile: TRadioButton;
    procedure btnAddFilesClick(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure rbAddFilesFromFoldersChange(Sender: TObject);
  private
    { private declarations }
    function FindFiles(const fileExpr: String; files: TStrings): Boolean;
  public
    { public declarations }
  end; 

var
  frmFileWizard: TfrmFileWizard;

implementation

uses editor;

{ TfrmFileWizard }

procedure TfrmFileWizard.btnAbortClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmFileWizard.btnAddFilesClick(Sender: TObject);
var
  tmp: TStringList;
  i: Integer;
  w: String;
  TargetEdit: TSynEdit;
begin
  if FileProfiles.Profiles_By_Page(frmEditor.IPSNotebook.ActivePageComponent)= nil then
  begin
    ShowMessage('No File-Profile Page active. Select a File-Profile-Page!');
    exit;
  end else
    TargetEdit := FileProfiles.Profiles_By_Page(frmEditor.IPSNotebook.ActivePageComponent).SynEdit;
  btnAddFiles.Enabled:=false;
  if not rbAddSingleFile.Checked then
  begin
    tmp:=TStringList.Create;
    tmp.Assign(FileUtil.FindAllFiles(edtFolder.Directory,'*',true));
    TargetEdit.Lines.BeginUpdate;
    for i:=0 to tmp.Count-1 do
    begin
      if FileExists(tmp[i]) then
      begin
        w:=StringReplace(tmp[i],edtFolder.Directory,'',[rfReplaceAll]);
        w:=StringReplace(w,ExtractFileName(tmp[i]),'',[rfReplaceAll]);
        w:=ExcludeTrailingBackslash(w);
        TargetEdit.Lines.Add(edtFolderCopyToPath.Text+w);  // istall path
        TargetEdit.Lines.Add(tmp[i]);                      // file
      end;
    end;

    tmp.Free;
    TargetEdit.Lines.EndUpdate;
  end
  else
  begin
    TargetEdit.Lines.Add(edtFileCopyToPath.Text);       // istall path
    TargetEdit.Lines.Add(edtFIle.FileName);             // file
  end;
  btnAddFiles.Enabled:=true;
end;

procedure TfrmFileWizard.rbAddFilesFromFoldersChange(Sender: TObject);
begin
  with Sender as TRadioButton do
    if Checked then
    begin
      GroupBox3.Enabled:=true;
      GroupBox4.Enabled:=false;
    end
    else
    begin
      GroupBox3.Enabled:=false;
      GroupBox4.Enabled:=true;
    end;
end;

function TfrmFileWizard.FindFiles(const fileExpr: String; files: TStrings): Boolean;
var
  sr2: TSearchRec;
  path2: string;
  extWanted: Boolean;
begin
  Result := True;
  files.Clear;
  path2 := ExtractFilePath(fileExpr);
  extWanted := true;
  if FindFirst(fileExpr, faArchive, sr2) = 0 then
  begin
    repeat
      if extWanted then
        files.Add(path2 + sr2.Name)
      else
        files.Add(ChangeFileExt(path2 + sr2.Name, ''));
    until FindNext(sr2) <> 0;
    FindClose(sr2);
  end
  else
    Result := False;
end;

initialization
  {$I fwiz.lrs}

end.

