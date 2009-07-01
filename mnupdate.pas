{ mnupdate.pas
  Copyright (C) Listaller Project 2008

  mnupdate.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  mnupdate.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Main unit of the updater application
unit mnupdate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, CheckLst, HTTPSend, IniFiles, MD5, LiCommon, updexec, LCLType,
  Process, Menus, trstrings, XMLRead, DOM, ldunit, ipkhandle, sqlite3ds, db;

type

  { TUMnForm }
  
  TAppNotes = record
  AppName: String;
  NVersion, OVersion: String;
  ID: String;
  end;

  TUMnForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    CheckListBox1: TCheckListBox;
    InfoMemo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ulist: Array of TStringList;
    ANotes: Array of TAppNotes;
    procedure CheckForUpdates;
  end; 

var
  UMnForm: TUMnForm;
  
const
  lp='/tmp/'; //Working directory of Listaller

implementation

{ TUMnForm }

procedure TUMnForm.BitBtn1Click(Sender: TObject);
begin
UExecFm.ShowModal;
end;

function FindChildNode(dn: TDOMNode; n: String): TDOMNode;
var i: Integer;
begin
Result:=nil;
for i:=0 to dn.ChildNodes.Count-1 do begin
if LowerCase(dn.ChildNodes.Item[i].NodeName)=LowerCase(n) then begin
Result:=dn.ChildNodes.Item[i].FirstChild;break;exit;end;
end;
end;

procedure TUMnForm.CheckForUpdates;
var
  HTTP: THTTPSend;
  tmp,h,sinfo,sources: TStringList;
  i,j,k: Integer;
  ok: Boolean;
  cnf: TIniFile;
  XNode: TDOMNode;
  Doc:      TXMLDocument;
  dsApp: TSQLite3Dataset; //AppDB connection
begin
  HTTP := THTTPSend.Create;
  tmp:= TStringList.Create;
  h:= TStringList.Create;
  sinfo:=TStringList.Create;
  sources:=TStringList.Create;
  cnf:=TInifile.Create(ConfigDir+'config.cnf');
    if cnf.ReadBool('Proxy','UseProxy',false) then begin
    HTTP.ProxyPort:=cnf.ReadString('Proxy','Port','');
    HTTP.ProxyHost:=cnf.ReadString('Proxy','Server','');
    end;
    cnf.Free;
  HTTP.UserAgent:='Listaller-Update';
  LoadForm.Show;

  Application.ProcessMessages;
  CheckListBox1.Clear;
  SetLength(ulist,0);
  SetLength(anotes,0);
  if not FileExists(RegDir+'updates.list') then exit;
  h.LoadFromFile(RegDir+'updates.list');
  if h.Count=1 then begin
  ShowMessage(strNoUpdates);
  exit;
  end;

  for k:=1 to h.Count-1 do
  if h[k][1]='-' then sources.Add(copy(h[k],2,pos(' <',h[k])-2));
  LoadForm.pBar1.Max:=sources.Count;
  Application.ProcessMessages;
  h.Free;

   writeLn('Opening database...');
    dsApp:= TSQLite3Dataset.Create(nil);
   with dsApp do
   begin
    FileName:=RegDir+'applications.db';
    TableName:='AppInfo';
    if not FileExists(FileName) then
    begin
     with FieldDefs do
      begin
        Clear;
        Add('Name',ftString,0,true);
        Add('ID',ftString,0,true);
        Add('Type',ftString,0,true);
        Add('Description',ftString,0,False);
        Add('Version',ftFloat,0,true);
        Add('Publisher',ftString,0,False);
        Add('Icon',ftString,0,False);
        Add('Profile',ftString,0,False);
        Add('AGroup',ftString,0,true);
        Add('InstallDate',ftDateTime,0,False);
        Add('Dependencies',ftMemo,0,False);
      end;
    CreateTable;
   end;
  end;
  dsApp.Active:=true;

  dsApp.SQL:='SELECT * FROM AppInfo';
  dsApp.Open;
  dsApp.Filtered:=true;
  dsApp.Edit;

  for k:=0 to sources.Count-1 do
  begin
  try
  SetLength(ulist,length(ulist)+1);
  ulist[length(ulist)-1]:= TStringlist.Create;

    HTTP.HTTPMethod('GET', sources[k]+'/'+'source.pin');
    tmp.LoadFromStream(HTTP.Document);
    tmp.SaveToFile('/tmp/source0.pin');
    
    ReadXMLFile(Doc,'/tmp/source0.pin');
    
    ok:=false;
    xnode:=Doc.DocumentElement.FindNode('application');

    dsApp.First;
    while not dsApp.EOF do
    begin
     if (xnode.Attributes.GetNamedItem('name').NodeValue=dsApp.FieldByName('Name').AsString) then begin ok:=true;break;end;
     dsApp.Next;
    end;

    if ok then begin
    HTTP.Clear;
    HTTP.HTTPMethod('GET', sources[k]+'/'+'sinfo.id');
    sleep(10);
    sinfo.LoadFromStream(HTTP.Document);

    for j:=0 to sinfo.Count-1 do
    if (j mod 3)=0 then begin

    if sinfo[j+1]<>MDPrint((MD5.MD5File(DeleteModifiers(SyblToPath(sinfo[j])),1024))) then begin
    ulist[length(ulist)-1].Add(sources[k]+'/'+DeleteModifiers(StringReplace(sinfo[j],'$','',[rfReplaceAll])));
    ulist[length(ulist)-1].Add(SyblToPath(sinfo[j+2]));
    end;

    end;
    LoadForm.pBar1.Position:=LoadForm.pBar1.Position+1;
    Application.ProcessMessages;

    if ulist[length(ulist)-1].Count>0 then begin
    CheckListBox1.Items.Add(xnode.Attributes.GetNamedItem('name').NodeValue);
    CheckListBox1.Checked[CheckListBox1.Items.Count-1]:=true;
    SetLength(anotes,length(anotes)+1);
    anotes[length(anotes)-1].NVersion:=FindChildNode(xnode,'version').NodeValue;
    anotes[length(anotes)-1].ID:=dsApp.FieldByName('ID').AsString;

    end else
    ulist[length(ulist)-1].Free;
  end;
  tmp.Free;
  sinfo.Free;
  finally
    HTTP.Free;
  end;
  end;

  dsApp.Free;
  writeLn('Database connection closed.');
if CheckListBox1.Items.Count<=0 then begin
//TrayIcon1.Icon.Handle:=Gtk2LoadStockPixmap(GTK_STOCK_PROPERTIES,GTK_ICON_SIZE_SMALL_TOOLBAR);
ShowMessage(strNoUpdates);
end else begin BitBtn1.Enabled:=true;
//TrayIcon1.Icon.Handle:=Gtk2LoadStockPixmap(GTK_STOCK_DIALOG_WARNING,GTK_ICON_SIZE_SMALL_TOOLBAR);
end;
LoadForm.Close;
end;

procedure TUMnForm.BitBtn2Click(Sender: TObject);
begin
CheckForUpdates;
end;

procedure TUMnForm.CheckListBox1Click(Sender: TObject);
begin
if CheckListBox1.ItemIndex>-1 then begin
InfoMemo.Enabled:=true;
InfoMemo.Lines.Clear;
InfoMemo.Lines.Add(strLogUpdInfo);
InfoMemo.Lines.Add(StringReplace(strFilesChanged,'%f',IntToStr((ulist[CheckListBox1.ItemIndex].Count div 2)),[rfReplaceAll]));
InfoMemo.Lines.Add(StringReplace(strUpdTo,'%v','"'+anotes[CheckListBox1.ItemIndex].NVersion+'"',[rfReplaceAll]));
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

procedure TUMnForm.FormCreate(Sender: TObject);
begin
if not DirectoryExists(RegDir) then begin
CreateDir(ExtractFilePath(RegDir));
CreateDir(RegDir);
end;

if LowerCase(paramstr(1))='-show' then Application.ShowMainForm:=true;
//Set icons
LoadStockPixmap(STOCK_REFRESH,ICON_SIZE_BUTTON,BitBtn2.Glyph);
LoadStockPixmap(STOCK_APPLY,ICON_SIZE_BUTTON,BitBtn1.Glyph);
//Translation
BitBtn2.Caption:=strCheckForUpd;
BitBtn1.Caption:=strInstUpd;
MenuItem1.Caption:=strQuitUpdater;
MenuItem2.Caption:=strShowUpdater;
end;

procedure TUMnForm.FormShow(Sender: TObject);
begin
  CheckForUpdates;
end;

procedure TUMnForm.MenuItem1Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TUMnForm.MenuItem2Click(Sender: TObject);
begin
  UMnForm.Show;
end;

initialization
  {$I mnupdate.lrs}

end.

