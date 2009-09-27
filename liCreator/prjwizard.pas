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
//** Graphical wizard to generate IPS scripts
unit prjwizard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, EditBtn, Grids, popupnotifier, FileCtrl,
  FileUtil, MD5, Menus, XMLRead, XMLWrite, DOM, editor, LCLType, CheckLst,
  LiCommon, SynEdit, LiTypes, IconLoader;

type

  { TfrmProjectWizard }

  //** Record for IPK package information
  TPackageFile = record
    FileName : String;
    FullName : String;
    CopyTo: String;
    Checksum: String;
    Modifier: String;
  end;

  //** Pointer to PackageFile
  PPackageFile = ^TPackageFile;

  TfrmProjectWizard = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    btnAssignShortDescription: TButton;
    btnProfileRemove: TButton;
    btnDistributionAdd: TButton;
    btnDistributionRemove: TButton;
    AddDepBtn: TButton;
    RmDepBtn: TButton;
    Button4: TButton;
    btnAddLangCode: TButton;
    btnProfileAdd: TButton;
    Button6: TButton;
    cgrIMethods: TCheckGroup;
    cbUseAppCMD: TCheckBox;
    ChkAddUDeps: TCheckBox;
    DependencyBox: TCheckListBox;
    chkShowInTerminal: TCheckBox;
    ComboBox1: TComboBox;
    cmbProfiles: TComboBox;
    DirectoryEdit1: TDirectoryEdit;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit8: TEdit;
    edtExec: TEdit;
    edtShortDescription: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    edtLangCode: TEdit;
    FileNameEdit1: TFileNameEdit;
    FileNameEdit2: TFileNameEdit;
    FileNameEdit3: TFileNameEdit;
    FileNameEdit4: TFileNameEdit;
    FileNameEdit5: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    GroupBox12: TGroupBox;
    grbProfiles: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    edtProfileName: TLabeledEdit;
    EdtNewDep: TLabeledEdit;
    lbDistributions: TListBox;
    lbProfiles: TListBox;
    lvPackageFiles: TListView;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Notebook1: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    Page4: TPage;
    Page5: TPage;
    Page6: TPage;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    tvShortDescriptions: TTreeView;
    tvDependencies: TTreeView;
    procedure AddDepBtnClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure btnAssignShortDescriptionClick(Sender: TObject);
    procedure btnProfileAddClick(Sender: TObject);
    procedure btnProfileRemoveClick(Sender: TObject);
    procedure btnDistributionAddClick(Sender: TObject);
    procedure btnDistributionRemoveClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnAddLangCodeClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure cbUseAppCMDChange(Sender: TObject);
    procedure cgrIMethodsItemClick(Sender: TObject; Index: integer);
    procedure ChkAddUDepsChange(Sender: TObject);
    procedure cmbProfilesChange(Sender: TObject);
    procedure cmbProfilesCloseUp(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure edtShortDescriptionChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label28Click(Sender: TObject);
    procedure lbDistributionsKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure lbProfilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvPackageFilesDblClick(Sender: TObject);
    procedure lvPackageFilesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RmDepBtnClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure tvDependenciesClick(Sender: TObject);
    procedure tvDependenciesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvShortDescriptionsClick(Sender: TObject);
    procedure tvShortDescriptionsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    function CreateScript(aType: TPkgType): TXMLDocument;
    procedure LoadFilesFromProfile(Profile: TList);
    procedure SaveFilesToProfile(Profile: TList);
    procedure ClearProfile(Profile: TList);
    procedure DeleteProfile(iIndex: Integer);
    procedure AddProfile(strName: String);
    function GetProfile(iIndex: integer): TList;
    function GetProfileCount: Integer;
    function GetProfileName(iIndex: integer): String;
    { private declarations }
  public
    { public declarations }
  end; 

var
  //** Project wizard main formular
  frmProjectWizard: TfrmProjectWizard;
  //** IPK/IPS type that should be created
  CreaType: TPkgType;

implementation

{ TfrmProjectWizard }

procedure TfrmProjectWizard.BitBtn2Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmProjectWizard.BitBtn1Click(Sender: TObject);
begin
  if CreaType=ptContainer then
  begin
    NoteBook1.PageIndex:=0;
    BitBtn3.Enabled:=false;
  end;
  BitBtn3.Caption:='  Next  ';
  if NoteBook1.PageIndex=2 then
     cmbProfilesChange(Sender);  // SaveFilesToProfile
  if (CreaType=ptDLink) and (NoteBook1.PageIndex=4) then
    NoteBook1.PageIndex:=NoteBook1.PageIndex-1;
  NoteBook1.PageIndex:=NoteBook1.PageIndex-1;
  if NoteBook1.PageIndex<=0 then
  begin
    BitBtn3.Enabled:=false;
    BitBtn1.Enabled:=false;
  end;
end;

procedure TfrmProjectWizard.AddDepBtnClick(Sender: TObject);
begin
if EdtNewDep.Text<>'' then
begin
  DependencyBox.Items.Add(EdtNewDep.Text);
  DependencyBox.Checked[DependencyBox.items.Count-1]:=true;
end;
end;

function TfrmProjectWizard.CreateScript(aType: TPkgType): TXMLDocument;
var
  s: String;
  i,j: Integer;
  xdoc: TXMLDocument;
  mn, hn, cnt, xn: TDOMNode;
const
  strBool: array[0..1] of String = ('false','true');
begin
  xdoc := TXMLDocument.create;
  Result := xdoc;
  mn:=xdoc.CreateElement('package');
  TDOMElement(mn).SetAttribute('version','0.8');
  case aType of
    ptLinstall:  TDOMElement(mn).SetAttribute('type','linstall');
    ptDLink:     TDOMElement(mn).SetAttribute('type','dlink');
    ptContainer: TDOMElement(mn).SetAttribute('type','container');
  end;
  XDoc.AppendChild(mn);

  if (aType=ptContainer) then
  begin
    hn := xdoc.CreateElement('application');
    mn.Appendchild(hn);

    hn:=xdoc.CreateElement('package');
    cnt:=xdoc.CreateTextNode(FileNameEdit4.FileName);
    hn.Appendchild(cnt);
    mn.ChildNodes.Item[0].AppendChild(hn);

    hn:=xdoc.CreateElement('InTerminal');
    if chkShowInTerminal.Checked then
      cnt:=xdoc.CreateTextNode(strBool[1])
    else
      cnt:=xdoc.CreateTextNode(strBool[0]);
    hn.Appendchild(cnt);
    mn.ChildNodes.Item[0].AppendChild(hn);

    exit;
  end;

  hn:=xdoc.CreateElement('architecture');
  cnt:=xdoc.CreateTextNode(Edit3.Text);
  hn.Appendchild(cnt);
  mn.AppendChild(hn);

  hn:=xdoc.CreateElement('application');
  TDOMElement(hn).SetAttribute('name', Edit1.Text);
  mn.AppendChild(hn);

  hn:=xdoc.CreateElement('version');
  cnt:=xdoc.CreateTextNode(Edit2.Text);
  hn.Appendchild(cnt);
  mn.ChildNodes.Item[1].AppendChild(hn);

  if (aType=ptLinstall)and(FileNameEdit1.FileName<>'') then // not used in dlink- and container-packages
  begin
    hn:=xdoc.CreateElement('license');
    cnt:=xdoc.CreateTextNode(FileNameEdit1.FileName);
    hn.Appendchild(cnt);
    mn.ChildNodes.Item[1].AppendChild(hn);
  end;

  hn:=xdoc.CreateElement('description');
  cnt:=xdoc.CreateTextNode(FileNameEdit2.FileName);
  hn.Appendchild(cnt);
  mn.ChildNodes.Item[1].AppendChild(hn);

  s:='';
  for i:=0 to lbDistributions.Items.Count-1 do
  begin
    if s='' then
      s:=lbDistributions.Items[i]
    else
      s:=s+','+lbDistributions.Items[i];
  end;

  hn:=xdoc.CreateElement('dsupport');
  cnt:=xdoc.CreateTextNode(s);
  hn.Appendchild(cnt);
  mn.ChildNodes.Item[1].AppendChild(hn);

  hn:=xdoc.CreateElement('author');
  cnt:=xdoc.CreateTextNode(Edit10.Text);
  hn.Appendchild(cnt);
  mn.ChildNodes.Item[1].AppendChild(hn);

  if FileExists(FileNameEdit5.FileName) then
  begin
    hn:=xdoc.CreateElement('icon');
    cnt:=xdoc.CreateTextNode(FileNameEdit5.FileName);
    hn.Appendchild(cnt);
    mn.ChildNodes.Item[1].AppendChild(hn);
  end;

 //Needed for testmode
  if cbUseAppCMD.Checked then
  begin
    hn:=xdoc.CreateElement('appcmd');
    cnt:=xdoc.CreateTextNode(edtExec.Text);
    hn.Appendchild(cnt);
    mn.ChildNodes.Item[1].AppendChild(hn);
  end;

  hn:=xdoc.CreateElement('sdesc');
  TDOMElement(hn).SetAttribute('std', tvShortDescriptions.Items[0].GetFirstChild.Text);
  mn.ChildNodes.Item[1].AppendChild(hn);
  for i:=1 to tvShortDescriptions.Items.Count-1 do
    if i mod 2 = 0 then
    begin
      xn:=xdoc.CreateElement(tvShortDescriptions.Items[i].Text);
      cnt:=xdoc.CreateTextNode(tvShortDescriptions.Items[i].GetFirstChild.Text);
      xn.Appendchild(cnt);
      hn.AppendChild(xn);
    end;

  hn:=xdoc.CreateElement('group');
  if ComboBox1.ItemIndex=9 then
    cnt:=xdoc.CreateTextNode('Other')
  else
    cnt:=xdoc.CreateTextNode(ComboBox1.Items[ComboBox1.ItemIndex]);
  hn.Appendchild(cnt);
  mn.ChildNodes.Item[1].AppendChild(hn);

  if (aType=ptDLink) then
  begin
    hn:=xdoc.CreateElement('desktopfiles');
    cnt:=xdoc.CreateTextNode(Edit12.Text);
    hn.Appendchild(cnt);
    mn.ChildNodes.Item[1].AppendChild(hn);
  end;

  hn:=xdoc.CreateElement('idName');
  cnt:=xdoc.CreateTextNode(Edit11.Text);
  hn.Appendchild(cnt);
  mn.AppendChild(hn);

  hn:=xdoc.CreateElement('disallow');
  if not cgrIMethods.Checked[0] then s:=s+';ioBase';
  if not cgrIMethods.Checked[1] then s:=s+';ioLocal';
  if not cgrIMethods.Checked[2] then s:=s+';ioTest';
  s:=copy(s,2,length(s));
  cnt:=xdoc.CreateTextNode(s);
  hn.Appendchild(cnt);
  mn.AppendChild(hn);
  s:='';

  if aType=ptLinstall then      // not used in dlink-packages
  begin
    for i:=0 to GetProfileCount-1 do
    begin
      hn:=xdoc.CreateElement('profile' + IntToStr(i+1));
      TDOMElement(hn).SetAttribute('id', IntToStr(i));
      cnt:=xdoc.CreateTextNode(GetProfileName(i));
      hn.Appendchild(cnt);
      mn.AppendChild(hn);
    end;
  end;

  //Dependencies
  hn:=xdoc.CreateElement('dependencies');
  j:=1;
  for i:=0 to DependencyBox.Items.Count-1 do
   if DependencyBox.Checked[i] then
  begin
   xn:=xdoc.CreateElement('d'+IntToStr(j));
   cnt:=xdoc.CreateTextNode(DependencyBox.Items[i]);
   xn.Appendchild(cnt);
   hn.AppendChild(xn);
   mn.AppendChild(hn);
   Inc(j);
  end;

  if ChkAddUDeps.Checked then
  begin

  for i:=0 to tvDependencies.Items.Count-1 do
  begin
    if tvDependencies.Items[i].HasChildren then
    begin
      if tvDependencies.Items[i].Text='DEB-System' then
        hn:=xdoc.CreateElement('DepDEB')
      else if tvDependencies.Items[i].Text='RPM-System' then
         hn:=xdoc.CreateElement('DepRPM')
      else
        hn:=xdoc.CreateElement('Dep'+tvDependencies.Items[i].Text);;
      for j:=0 to tvDependencies.Items[i].Count-1 do
      begin
        xn:=xdoc.CreateElement('d'+IntToStr(j+1));
        cnt:=xdoc.CreateTextNode(tvDependencies.Items[i].Items[j].Text);
        xn.Appendchild(cnt);
        hn.AppendChild(xn);
      end;
      mn.AppendChild(hn);
    end;
  end;

  end; //END CB

end;

procedure TfrmProjectWizard.BitBtn3Click(Sender: TObject);
var
  i,j,k: Integer;
  aTreeNode: TTreeNode;
  Profile: TList;
  TargetEdit: TSynEdit;
  s: String;
  sl: TStringList;
//XML
  xdoc: TXMLDocument;
begin
  if CreaType=ptLinstall then
  begin
  if NoteBook1.PageIndex=4 then
  begin
    //Create Scripts
    with frmEditor do
    begin
      //Files
      NewBlank;
      for j:=0 to GetProfileCount-1 do
      begin
        Profile := GetProfile(j);
        TargetEdit := editor.FileProfiles.AddProfile(j).SynEdit;
        s:=''; //S is used for current file path
        for i:=0 to Profile.Count-1 do
        begin
         if PPackageFile(Profile[i])^.CopyTo <> s then
         begin
          s:=PPackageFile(Profile[i])^.CopyTo;
          TargetEdit.Lines.Add('>' + s);
         end;
          if PPackageFile(Profile[i])^.Modifier<>'' then
            TargetEdit.Lines.Add(PPackageFile(Profile[i])^.FullName +' '+PPackageFile(Profile[i])^.Modifier)
          else
            TargetEdit.Lines.Add(PPackageFile(Profile[i])^.FullName);
          //FileInfo.Add(PPackageFile(Profile[i])^.Checksum);
        end;
      end;

      //Script
      xdoc := CreateScript(ptLinstall);
   
      writeXMLFile(xDoc,'/tmp/litmp.xml');
      XDoc.Free;
      frmEditor.MainScriptEdit.Lines.LoadFromFile('/tmp/litmp.xml');
      DeleteFile('/tmp/litmp.xml');
      // frmEditor.Page2.TabVisible:=true;
    end;
    Close;
    exit;
  end;

  case NoteBook1.PageIndex of
  0: begin
       NoteBook1.PageIndex:=NoteBook1.PageIndex+1;
       exit;
     end;
  1: begin
       aTreeNode := tvShortDescriptions.Items.GetFirstNode;
       repeat
         if not aTreeNode.HasChildren then
         begin
           ShowMessage('Please define a short description of your application ('
                       +aTreeNode.Text+')!');
           exit;
         end;
         aTreeNode := aTreeNode.GetNextSibling;
       until aTreeNode=nil;
       if not FileExists(FileNameEdit2.FileName) then
       begin
         ShowMessage('Set the path to an long description file of your application to continue!');
         exit;
       end;

       if (cgrIMethods.Checked[2])
       and(not cbUseAppCMD.Checked) then
       begin
        ShowMessage('Listaller''s tesmode requires an start command for your application!');
        exit;
       end;

       // set settings for package files:
       Edit6.Caption:='$INST/'+Edit1.Text;
       Edit7.Caption:=Edit6.Caption;
       cmbProfiles.Tag:=-1;
       cmbProfiles.Items.Assign(lbProfiles.Items);
       if cmbProfiles.Items.Count>0 then cmbProfiles.ItemIndex:=0;
       cmbProfilesChange(Sender);

       NoteBook1.PageIndex:=NoteBook1.PageIndex+1;

       exit;
     end;
  2: begin
      for i:=0 to lbDistributions.Count-1 do
         tvDependencies.Items.Add(nil,lbDistributions.Items[i]);

         cmbProfilesChange(Sender);
       //Load dependency list
       if DependencyBox.Count<=0 then
       begin
       sl:=TStringList.Create;
       for j:=0 to GetProfileCount-1 do
      begin
        Profile := GetProfile(j);
        for i:=0 to Profile.Count-1 do
        begin
         if FileIsExecutable(PPackageFile(Profile[i])^.FullName) then
          begin
           GetLibDepends(PPackageFile(Profile[i])^.FullName,sl);
          end;
        end;
      end;

      for j:=0 to GetProfileCount-1 do
      begin
        Profile := GetProfile(j);
        for i:=0 to Profile.Count-1 do
        begin
          for k:=0 to sl.Count-1 do
           if ExtractFileName(PPackageFile(Profile[i])^.FullName)=ExtractFileName(sl[k]) then
           sl.Delete(k);
        end;
      end;

       DependencyBox.Items.Assign(sl);
       sl.Free;
       for i:=0 to DependencyBox.Items.Count-1 do
       DependencyBox.Checked[i]:=true;
      end;

       NoteBook1.PageIndex:=NoteBook1.PageIndex+1;
     end;
  3: begin
      cmbProfilesChange(Sender); //SaveFilesToProfile
       if lvPackageFiles.Items.Count<1 then
       begin
         ShowMessage('Select some files that will be installed.'#13'- a package without files is useless. ;-)');
         exit;
       end;
       NoteBook1.PageIndex:=NoteBook1.PageIndex+1;
       //
       BitBtn3.Caption:='Generate script file';
       with Memo1.Lines do
       begin
         Clear;
         Add('Information about the new setup:');
         Add('#');
         Add('Application:');
         Add('------------');
         Add('Name:         '+Edit1.Text);
         Add('Version:      '+Edit2.Text);
         Add('Group:        '+ComboBox1.Items[ComboBox1.ItemIndex]);
         if FileExists(FileNameEdit1.FileName) then
           Add('License-file: '+FileNameEdit1.FileName)
         else
           Add('License-file: <none>');
         if FileExists(FileNameEdit2.FileName) then
           Add('LDesc-file:   '+FileNameEdit2.FileName)
         else
           Add('LDesc-file:   <none>');
         Add('Short-desc.:  ');
         if tvShortDescriptions.Items<>nil then
         begin
           for i:=0 to tvShortDescriptions.Items.Count-1 do
             if i mod 2 = 0 then
               Add('              '+tvShortDescriptions.Items[i].Text+': '+tvShortDescriptions.Items[i].GetFirstChild.Text);

         end
         else
           Add('              <none>');
         Add('Package:');
         Add('--------');
         Add('Architecture:     '+Edit3.Text);
         Add('Supported Distros:');
         for i:=0 to lbDistributions.Count-1 do
           Add('                  '+lbDistributions.Items[i]);

         Add('Dependencies:');
         Add('-------------');
         for i:=0 to tvDependencies.Items.Count-1 do
         begin
           if tvDependencies.Items[i].HasChildren then
           begin
             Add('             '+tvDependencies.Items[i].Text);
             for j:=0 to tvDependencies.Items[i].Count-1 do
               Add('                 '+tvDependencies.Items[i].Items[j].Text);
           end;
         end;
         Add('Files:');
         for j:=0 to GetProfileCount-1 do
         begin
           Add('--- #' + IntToStr(j) + ' '+ GetProfileName(j));
           Profile := GetProfile(j);
           for i:=0 to Profile.Count-1 do
           begin
             Add('      ' + PPackageFile(Profile[i])^.FileName + '||' +
                 PPackageFile(Profile[i])^.FullName + '||' +
                 PPackageFile(Profile[i])^.CopyTo + '||' +
                 PPackageFile(Profile[i])^.Modifier + '||' +
                 PPackageFile(Profile[i])^.Checksum);
          end;
         end;
         SelectFirst;
       end;
     end;
  end;
end; //End of Normal

 if CreaType=ptDLink then begin
   case NoteBook1.PageIndex of
   1: begin
      if tvShortDescriptions.Items.Count<=1 then begin ShowMessage('Please define a short description for your application!');exit;end;
      if not FileExists(FileNameEdit2.FileName) then begin ShowMessage('Set the path to an long description file of your application to continue!');exit;end;
        NoteBook1.PageIndex:=2;
        for i:=0 to lbDistributions.Count-1 do
         tvDependencies.Items.Add(nil,lbDistributions.Items[i]);
      end;
   2: begin
        if tvDependencies.Items.Count<=(2+lbDistributions.Count) then
        begin
          ShowMessage('The dlink-package should have at least one dependency!');
          exit;
        end;
        NoteBook1.PageIndex:=4;
        BitBtn3.Caption:='Generate script file';

        with Memo1.Lines do
        begin
          Add('Information about the new setup:');
          Add('#');
          Add('Application:');
          Add('------------');
          Add('Name:         '+Edit1.Text);
          Add('Version:      '+Edit2.Text);
          Add('Group:        '+ComboBox1.Items[ComboBox1.ItemIndex]);
          if FileExists(FileNameEdit1.FileName) then
            Add('License-file: '+FileNameEdit1.FileName)
          else
            Add('License-file: <none>');
          if FileExists(FileNameEdit2.FileName) then
            Add('LDesc-file:   '+FileNameEdit2.FileName)
          else
            Add('LDesc-file:   <none>');
          Add('Short-desc.:  ');
          if tvShortDescriptions.Items<>nil then
          begin
            for i:=0 to tvShortDescriptions.Items.Count-1 do
              if i mod 2 = 0 then
                Add('              '+tvShortDescriptions.Items[i].Text+': '+tvShortDescriptions.Items[i].GetFirstChild.Text);
          end
          else
            Add('              <none>');
          Add('Package:');
          Add('--------');
          Add('Architecture:     '+Edit3.Text);
          Add('Supported Distros:');
          for i:=0 to lbDistributions.Count-1 do
            Add('                  '+lbDistributions.Items[i]);

          Add('Dependencies:');
          Add('-------------');
          for i:=0 to tvDependencies.Items.Count-1 do
          begin
            if tvDependencies.Items[i].HasChildren then
            begin
              Add('             '+tvDependencies.Items[i].Text);
              for j:=0 to tvDependencies.Items[i].Count-1 do
                Add('                 '+tvDependencies.Items[i].Items[j].Text);
            end;
          end;
        end;
      end;
   4: begin
        //Create Scripts
        with frmEditor do
          begin
          //Script
          xdoc := CreateScript(ptDLink);
          writeXMLFile(xDoc,'/tmp/litmp.xml');
          XDoc.Free;
          MainScriptEdit.Lines.LoadFromFile('/tmp/litmp.xml');
          DeleteFile('/tmp/litmp.xml');
          Page2.TabVisible:=false;
        end;
        Close;
        exit;
      end;
    end;
  end; //End of DGet

  if CreaType=ptContainer then
  begin
    xdoc := CreateScript(ptContainer);
    writeXMLFile(xDoc,'/tmp/litmp.xml');
    XDoc.Free;
    frmEditor.MainScriptEdit.Lines.LoadFromFile('/tmp/litmp.xml');
    DeleteFile('/tmp/litmp.xml');
    // frmEditor.Page2.TabVisible:=false;
    Close;
  end; //End of LOKI
end;

procedure TfrmProjectWizard.btnAssignShortDescriptionClick(Sender: TObject);
begin
  with tvShortDescriptions do
  begin
    if Selected<>nil then
    begin
      if (Selected.HasChildren) then
       Selected.DeleteChildren;
      Items.AddChild(Selected,edtShortDescription.Text);
    end;
  end;
end;

procedure TfrmProjectWizard.DeleteProfile(iIndex: Integer);
var
  Profile: TList;
begin
  Profile := lbProfiles.Items.Objects[iIndex] as  TList;
  ClearProfile(Profile);
  Profile.Free;
  lbProfiles.Items.Delete(iIndex);
end;

function TfrmProjectWizard.GetProfileName(iIndex: integer): String;
begin
  if iIndex<lbProfiles.Count then
    Result := lbProfiles.Items[iIndex]
  else
    Result := '';
end;

function TfrmProjectWizard.GetProfileCount: Integer;
begin
  Result := lbProfiles.Count;
end;

procedure TfrmProjectWizard.AddProfile(strName: String);
begin
  lbProfiles.Items.AddObject(strName, TList.Create);
end;

function TfrmProjectWizard.GetProfile(iIndex: integer): TList;
begin
  Result := nil;
  if (iIndex>-1) and (iIndex<lbProfiles.Count) then
  begin
    Result := lbProfiles.Items.Objects[iIndex] as TList;
  end;
end;

procedure TfrmProjectWizard.btnProfileAddClick(Sender: TObject);
begin
  if not (Trim(edtProfileName.Text)='') then
    AddProfile(edtProfileName.Text);
end;

procedure TfrmProjectWizard.btnProfileRemoveClick(Sender: TObject);
begin
  if (lbProfiles.Items.Count<=1) then
  begin
    ShowMessage('There must be at least 1 profile! Create another one before deleting.');
    exit;
  end;
  if (lbProfiles.ItemIndex>-1) then
  begin
    DeleteProfile(lbProfiles.ItemIndex);
  end;
end;

procedure TfrmProjectWizard.btnDistributionAddClick(Sender: TObject);
begin
  if StringReplace(Edit5.Text,' ','',[rfReplaceAll])<>'' then
    lbDistributions.Items.Add(Edit5.Text);
end;

procedure TfrmProjectWizard.btnDistributionRemoveClick(Sender: TObject);
begin
  if lbDistributions.ItemIndex>-1 then
    lbDistributions.Items.Delete(lbDistributions.ItemIndex);
end;

procedure TfrmProjectWizard.Button4Click(Sender: TObject);
var
  aNode: TTreeNode;
begin
  aNode := tvDependencies.Selected;
  if not (aNode=nil) then
  begin
    if not (aNode.Parent=nil) then aNode := aNode.Parent;
    if (StringReplace(Edit8.Text,' ','',[rfReplaceAll])<>'') and
         ((pos('<',Edit8.Text)>0) or
         ((pos('http://',Edit8.Text)<=0)and(pos('ftp://',Edit8.Text)<=0))) then
      tvDependencies.Items.AddChild(aNode,Edit8.Text)
    else
      ShowMessage('Please add the package-name!');
  end;
end;

procedure TfrmProjectWizard.btnAddLangCodeClick(Sender: TObject);
begin
  if (Length(edtLangCode.Text)>1)and(edtLangCode.Text<>'') then
    tvShortDescriptions.Items.Add(nil,LowerCase(edtLangCode.Text));
end;

procedure TfrmProjectWizard.ClearProfile(Profile: TList);
var
  i: Integer;
begin
  for i:=Profile.Count-1 downto 0 do
  begin
    Dispose(PPackageFile(Profile[i]));
    Profile.Delete(i);
  end;
end;

procedure TfrmProjectWizard.LoadFilesFromProfile(Profile: TList);
var
  i: Integer;
  PackageFile: PPackageFile;
  aItem : TListItem;
begin
  lvPackageFiles.Items.Clear;
  for i:=0 to Profile.Count-1 do
  begin
    aItem := lvPackageFiles.Items.Add;
    PackageFile := Profile[i];
    aItem.Caption:=PackageFile^.FileName;
    aItem.SubItems.Add(PackageFile^.FullName);
    aItem.SubItems.Add(PackageFile^.CopyTo);
    aItem.SubItems.Add(PackageFile^.Modifier);
    aItem.SubItems.Add(PackageFile^.Checksum);
  end;
end;

procedure TfrmProjectWizard.SaveFilesToProfile(Profile: TList);
var
  i: Integer;
  PackageFile: PPackageFile;
begin
  ClearProfile(Profile);
  for i:=0 to lvPackageFiles.Items.Count-1 do
  begin
    PackageFile := New(PPackageFile);
    PackageFile^.FileName:=lvPackageFiles.Items[i].Caption;
    PackageFile^.FullName:=lvPackageFiles.Items[i].SubItems[0];
    PackageFile^.CopyTo:=lvPackageFiles.Items[i].SubItems[1];
    PackageFile^.Modifier:=lvPackageFiles.Items[i].SubItems[2];
    PackageFile^.Checksum:=lvPackageFiles.Items[i].SubItems[3];
    Profile.Add(PackageFile);
  end;
end;

procedure TfrmProjectWizard.Button6Click(Sender: TObject);
var
  tmp: TStringList;
  i,j: Integer;
  w: String;
begin
  if not RadioButton2.Checked then
  begin
    tmp:=TStringList.Create;
    tmp.Assign(FileUtil.FindAllFiles(DirectoryEdit1.Directory,'*',true));
    j:=0;
    i:=0;
    for j:=lvPackageFiles.Items.Count to tmp.Count+lvPackageFiles.Items.Count-1 do
    begin
      if FileExists(tmp[i]) then
      begin
        lvPackageFiles.Items.Add;
        lvPackageFiles.Items[j].Caption:=ExtractFileName(tmp[i]);
        lvPackageFiles.Items[j].SubItems.Add(tmp[i]);

        w:=StringReplace(tmp[i],DirectoryEdit1.Directory,'',[rfReplaceAll]);
        w:=StringReplace(w,ExtractFileName(tmp[i]),'',[rfReplaceAll]);
        w:=ExcludeTrailingBackslash(w);
        lvPackageFiles.Items[j].SubItems.Add(Edit6.Text+w);
        lvPackageFiles.Items[j].SubItems.Add('');
        lvPackageFiles.Items[j].SubItems.Add(MD5.MDPrint(MD5.MD5File(tmp[i],1024)));
      end;
      Inc(i);
    end;
    tmp.Free;

  end else
  begin
    lvPackageFiles.Items.Add;
    i:=lvPackageFiles.Items.Count-1;
    lvPackageFiles.Items[i].Caption:=ExtractFileName(FileNameEdit3.FileName);
    lvPackageFiles.Items[i].SubItems.Add(FileNameEdit3.FileName);
    lvPackageFiles.Items[i].SubItems.Add(Edit7.Text);
    lvPackageFiles.Items[i].SubItems.Add('');
    lvPackageFiles.Items[i].SubItems.Add(MD5.MDPrint(MD5.MD5File(FileNameEdit3.FileName,1024)));
  end;
end;

procedure TfrmProjectWizard.cbUseAppCMDChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
     edtExec.Enabled:=true
  else
     edtExec.Enabled:=false;
end;

procedure TfrmProjectWizard.cgrIMethodsItemClick(Sender: TObject; Index: integer
  );
begin

end;

procedure TfrmProjectWizard.ChkAddUDepsChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Enabled then
  begin
   Label20.Enabled:=true;
   GroupBox10.Enabled:=true;
   tvDependencies.Enabled:=true;
  end else
  begin
   Label20.Enabled:=false;
   GroupBox10.Enabled:=false;
   tvDependencies.Enabled:=false;
  end;
end;

procedure TfrmProjectWizard.cmbProfilesChange(Sender: TObject);
begin
  if cmbProfiles.Tag>-1 then
    SaveFilesToProfile(TList(cmbProfiles.Items.Objects[cmbProfiles.Tag]));
  LoadFilesFromProfile(TList(cmbProfiles.Items.Objects[cmbProfiles.ItemIndex]));
  cmbProfiles.Tag:=cmbProfiles.ItemIndex;   // copy current itemindex to tag
end;

procedure TfrmProjectWizard.cmbProfilesCloseUp(Sender: TObject);
begin

end;

procedure TfrmProjectWizard.Edit1Change(Sender: TObject);
begin
  Edit11.Caption:=LowerCase(Edit1.Text)+StringReplace(Edit2.Text,'.','',[rfReplaceAll]);
end;

procedure TfrmProjectWizard.Edit2Change(Sender: TObject);
begin
  Edit11.Caption:=LowerCase(Edit1.Text)+StringReplace(Edit2.Text,'.','',[rfReplaceAll]);
end;

procedure TfrmProjectWizard.edtShortDescriptionChange(Sender: TObject);
begin
 (* with tvShortDescriptions do
  begin
    if Selected<>nil then
    begin
      if (Selected.HasChildren) then
        Selected.GetFirstChild.Text := edtShortDescription.Text
      else
        Items.AddChild(Selected, edtShortDescription.Text);
    end;
  end;*)
end;

procedure TfrmProjectWizard.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
var
  i: Integer;
begin
  for i:=lbProfiles.Count-1 downto 0 do
  begin
    DeleteProfile(i);
  end;
end;

procedure TfrmProjectWizard.FormCreate(Sender: TObject);
begin
  LoadStockPixmap(STOCK_PROJECT_OPEN,ICON_SIZE_MENU,SpeedButton4.Glyph);
end;

procedure TfrmProjectWizard.FormShow(Sender: TObject);
begin
  NoteBook1.PageIndex:=0;
  BitBtn1.Enabled:=false;
  BitBtn3.Caption:='  Next  ';
  AddProfile('Standard');
  Edit3.Text:=GetSystemArchitecture;
end;

procedure TfrmProjectWizard.Label28Click(Sender: TObject);
begin

end;

procedure TfrmProjectWizard.lbDistributionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=46 then
  begin
    btnDistributionRemoveClick(Sender);
  end;
  Key := 0;
end;

procedure TfrmProjectWizard.lbProfilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=46 then
  begin
    btnProfileRemoveClick(Sender);
  end;
  Key := 0;
end;

procedure TfrmProjectWizard.lvPackageFilesDblClick(Sender: TObject);
var s: String;
begin
  s:=InputBox('Add modifier','Please type in the modifier, that you want to use!','');
  if (s<>'') then
    lvPackageFiles.Selected.SubItems[2]:=s
end;

procedure TfrmProjectWizard.lvPackageFilesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  if Key=46 then                     // Key "DEL"
  begin
    for i:=lvPackageFiles.Items.Count-1 downto 0 do
    begin
      if lvPackageFiles.Items[i].Selected then
        lvPackageFiles.Items.Delete(i);
    end;
    Key := 0;
  end;
end;

procedure TfrmProjectWizard.MenuItem1Click(Sender: TObject);
begin
  if lvPackageFiles.Selected<>nil then
    lvPackageFiles.Selected.Delete;
end;

procedure TfrmProjectWizard.MenuItem2Click(Sender: TObject);
begin

end;

procedure TfrmProjectWizard.MenuItem4Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmProjectWizard.MenuItem5Click(Sender: TObject);
var s: String;
begin
  s:='List of available placeholders:';
  s:=s+#13'§INST : Points to "/opt/appfiles" in ROOT installation, otherwise to "$HOME/applications/files"';
  s:=s+#13'$INST-X : Replaced by "/usr/share" on ROOT installation, "$HOME/applications/files" on installation in Home-directory.';
  s:=s+#13'$OPT : Points to "/opt" if user is ROOT, otherwise it does the same as $INST';
  s:=s+#13'$BIN : Pointer to "/usr/bin" if ROOT, else "$HOME/applications/binary"';
  s:=s+#13'$LIB : Pointer to "/usr/lib64" (on 64bit machines) or to "/usr/lib" (all others) if ROOT, else "$HOME/applications/files/lib64" or "$HOME/applications/files/lib"';
  s:=s+#13'$APP : Pointer to "/usr/share/applications", if ROOT, else "$HOME/applications"';
  s:=s+#13'$ICON-# : Points to "/usr/share/icons/hicolor/#x#/apps" if user is ROOT, else to "$HOME/applications/files/icons/#x#". The "#" symbol has to be replaced with the icon size: 16, 24, 32, 48, 64, 128, 265 pixels.';
  s:=s+#13'$PIX : Symbol for general pixmaps. Points to "/usr/share/pixmaps" if ROOT, else "$HOME/applications/files/icons/common"';
  s:=s+#13'($HOME stands for the home directory of the current user. It is NO Listaller placeholder!)';
  ShowMessage(s);
end;

procedure TfrmProjectWizard.MenuItem6Click(Sender: TObject);
begin
  ShowMessage('List of modifiers:'#13'<chmod:xxx> : Assign the rights xxx to the file.'#13'<s> : Mark file as shared file'#13'<setvars> : Go inside the file and replace all placeholders with their current values'#13#13'If possible you should not use any modifier!');
end;

procedure TfrmProjectWizard.RadioButton1Change(Sender: TObject);
begin
  if (Sender as TRadioButton).Checked then
  begin
    GroupBox4.Enabled:=true;
    GroupBox7.Enabled:=false;
  end
  else
  begin
    GroupBox4.Enabled:=false;
    GroupBox7.Enabled:=true;
  end;
end;

procedure TfrmProjectWizard.RmDepBtnClick(Sender: TObject);
begin
if DependencyBox.ItemIndex>-1 then
begin
  DependencyBox.Items.Delete(DependencyBox.ItemIndex);
end;
end;

procedure TfrmProjectWizard.SpeedButton1Click(Sender: TObject);
begin
  Notebook1.PageIndex:=1;
  FileNameEdit2.Enabled:=true;
  CreaType:=ptLinstall;
  Label30.Visible:=false;
  Edit12.Visible:=false;
  Label29.Visible:=false;
  BitBtn3.Enabled:=true;
  BitBtn1.Enabled:=true;
end;

procedure TfrmProjectWizard.SpeedButton2Click(Sender: TObject);
begin
  CreaType:=ptDLink;
  BitBtn3.Enabled:=true;
  Label30.Visible:=true;
  Edit12.Visible:=true;
  Label29.Visible:=true;
  FileNameEdit1.Enabled:=false;
  Notebook1.PageIndex:=1;
  BitBtn1.Enabled:=true;
end;

procedure TfrmProjectWizard.SpeedButton3Click(Sender: TObject);
begin
  CreaType:=ptContainer;
  BitBtn3.Caption:='Generate script';
  BitBtn3.Enabled:=true;
  NoteBook1.PageIndex:=5;
  BitBtn1.Enabled:=true;
end;

procedure TfrmProjectWizard.SpeedButton4Click(Sender: TObject);
begin
  frmEditor.mnuFileLoadIPSClick(nil);
  Close;
end;

procedure TfrmProjectWizard.tvDependenciesClick(Sender: TObject);
begin

end;

procedure TfrmProjectWizard.tvDependenciesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key=46 then  // 'DEL'-key
  begin
    if tvDependencies.Selected =nil then exit;
    if not (tvDependencies.Selected.Parent = nil) then
      tvDependencies.Items.Delete(tvDependencies.Selected);
  end;
end;

procedure TfrmProjectWizard.tvShortDescriptionsClick(Sender: TObject);
begin
  if (tvShortDescriptions.Selected<>nil) then
  begin
    if not (tvShortDescriptions.Selected.Parent = nil) then
      tvShortDescriptions.Selected := tvShortDescriptions.Selected.Parent;
    Label12.Caption:='Description for ~'+tvShortDescriptions.Selected.Text+'~:';
    if tvShortDescriptions.Selected.HasChildren then
      edtShortDescription.Text := tvShortDescriptions.Selected.GetFirstChild.Text;
  end;
end;

procedure TfrmProjectWizard.tvShortDescriptionsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=46 then
  begin
    if tvShortDescriptions.Selected = nil then exit;           // exit, when nothing selected
    if tvShortDescriptions.Selected.AbsoluteIndex=0 then exit; // exit, when 1st node selected
    tvShortDescriptions.Items.Delete(tvShortDescriptions.Selected);
  end;
end;

initialization
  {$I prjwizard.lrs}

end.

