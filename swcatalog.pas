{ swcatalog.pas
  Copyright (C) Listaller Project 2008-2009

  manager.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  swcatalog.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License v3
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains code that is used by the catalogue-GUI
unit swcatalog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, manager, HTTPSend, XMLRead, DOM, IniFiles, common,
  trstrings, blcksock, Process, ExtCtrls, LCLType;

type

  { TSCForm }

  TSCForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CView: TListView;
    Label4: TLabel;
    Memo1: TMemo;
    MnProgress: TProgressBar;
    DLProgress: TProgressBar;
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    HQBox: TToggleBox;
    Splitter1: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CViewClick(Sender: TObject);
    procedure CViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HQBoxChange(Sender: TObject);
  private
    { private declarations }
    SWLLength: Integer;
    current: Integer;
    //** List of all available catalogue servers
    lsrv: TStringList;
    procedure OpenCatalog;
    procedure GetCatalog;
    procedure HookSock(Sender: TObject; Reason: THookSocketReason;
const Value: string);
  public
    { public declarations }
    HTTP   : THTTPSend;
    PB1    : Boolean;
  end; 

  TCTLEntry = class(TListEntry)
  private
    InfoBtn: TBitBtn;
    //** Real name
    rnm:     String;
    //** HTTP-get object
    xHTTP: THTTPSend;
    //** ID number
    xNR: Integer;
    procedure DLHookSock(Sender: TObject; Reason: THookSocketReason;const Value: string);
    procedure InstallClick(Sender: TObject);
    procedure InfoClick(Sender: TObject);
   public
    //** Number of the exemplar
    property eNR: Integer read xNR write xNR;
    //** Constructor
    constructor Create(AOwner: TComponent); override;
    //** Destructor
    destructor  Destroy; override;
  end;

var
 //** Path to the current catalogue
  CatalogPath: String='http://listaller.nlinux.org/repo/catalogue/';
const
 //** Path to our server
  LiSrvPath='http://listaller.nlinux.org/repo/';
var
  SCForm: TSCForm;
  SWList: Array of TCTLEntry;
  //
  fActiv : Boolean=true;

implementation

{ TSCForm }

constructor TCTLEntry.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
self.Height:=54;

with Graphic do begin
Top:=8;
Width:=32;
Height:=32;
end;

with AppLabel do begin
Font.Size:=14;
Top:=4;
Left:=54;
end;

with mnLabel do begin
Parent:=Self;
AutoSize:=true;
Top:=20;
Left:=66;
end;

with UnButton do begin
Parent:=self;
Height:=26;
Width:=80;
Caption:=strInstallNow;
OnClick:=@InstallClick;
Top:=16;
Left:=self.Width-90;
AutoSize:=true;
LoadStockPixmap(STOCK_APPLY,ICON_SIZE_BUTTON,Glyph);
end;
UnButton.Left:=Width-UnButton.Width-8;

InfoBtn:=TBitBtn.Create(self);
InfoBtn.Parent:=self;
with InfoBtn do begin
InfoBtn.Caption:='Info';
InfoBtn.AutoSize:=true;
LoadStockPixmap(STOCK_DIALOG_INFO,ICON_SIZE_BUTTON, Glyph);
InfoBtn.OnClick:=@InfoClick;
Height:=28;
Top:=16;
Anchors:=[akBottom,akRight];
end;
InfoBtn.Left:=self.Width-InfoBtn.Width-UnButton.Width-16;
end;

destructor TCTLEntry.Destroy;
begin
InfoBtn.Free;
inherited Destroy;
end;

procedure TCTLEntry.DLHookSock(Sender: TObject; Reason: THookSocketReason;
const Value: string);
begin
Application.ProcessMessages;
//HTTP
if Assigned(xHTTP) then begin
if (xHttp.Document.Size>0) then begin
  SCForm.DLProgress.Max:=xHTTP.DownloadSize;
  SCForm.DLProgress.Position:=xHTTP.Document.Size;
 end;
end;
end;

procedure TCTLEntry.InfoClick(Sender: TObject);
var cdir: String;tmp: TStringList;i,j: Integer;
begin
   case id of
     0: cdir:='all';
     1: cdir:='education';
     2: cdir:='office';
     3: cdir:='development';
     4: cdir:='graphic';
     5: cdir:='network';
     6: cdir:='games';
     7: cdir:='system';
     8: cdir:='multimedia';
     9: cdir:='other';
    end;
    SCForm.HQBox.Visible:=true;
if FileExists('/tmp/listaller/catalogue/'+cdir+'/texts/'+rnm+'.txt') then begin
tmp:=TStringList.Create;
tmp.LoadFromFile('/tmp/listaller/catalogue/'+cdir+'/texts/'+rnm+'.txt');
for i:=0 to tmp.Count-1 do
if (LowerCase(tmp[i])=copy(GetEnvironmentVariable('LANG'), 1, 2))
and (tmp[i+1]='------') then break;

SCForm.Memo1.Lines.Clear;
SCForm.Memo1.Lines.Add(AppLabel.Caption);
SCForm.Memo1.Lines.Add('--------');
for j:=i+2 to tmp.Count-1 do
if tmp[j]<>'------' then
SCForm.Memo1.Lines.Add(tmp[j]) else break;
if i+1=tmp.Count then begin
for i:=0 to tmp.Count-1 do
if tmp[i]<>'------' then
SCForm.Memo1.Lines.Add(tmp[i]) else break;
tmp.Free;
 end;
SCForm.Memo1.SelStart:=0;
SCForm.Memo1.Visible:=true;
SCForm.HQBox.Checked:=true;
end else begin SCForm.Memo1.Lines.Clear;SCForm.Memo1.Lines.Add(strNoInfo);SCForm.Memo1.Visible:=true;SCForm.HQBox.Checked:=true;end;
end;

procedure TCTLEntry.InstallClick(Sender: TObject);
var tmp: TStringList;cdir: String;t: TProcess;i: Integer;
begin
   case id of
     0: cdir:='all';
     1: cdir:='education';
     2: cdir:='office';
     3: cdir:='development';
     4: cdir:='graphic';
     5: cdir:='network';
     6: cdir:='games';
     7: cdir:='system';
     8: cdir:='multimedia';
     9: cdir:='other';
    end;

    SCForm.Label2.Caption:=strDLSetUp;
    //Workaround for LazProblem
    xHTTP:=THTTPSend.Create;
    xHTTP.Sock.OnStatus:=@DLHookSock;
    xHTTP.keepAlive:=true;
     //Set HTTP settings
    xHTTP.ProxyPort:=SCForm.HTTP.ProxyPort;
    xHTTP.ProxyHost:=SCForm.HTTP.ProxyHost;
    SCForm.PB1:=true;
   tmp:=tStringList.Create;
   xHTTP.HTTPMethod('GET', catalogpath+cdir+'/'+rnm+'.todo');
   tmp.LoadFromStream(xHTTP.Document);
   if not DirectoryExists('/tmp/listaller') then CreateDir('/tmp/listaller/');
    if not DirectoryExists('/tmp/listaller/catalogue') then CreateDir('/tmp/listaller/catalogue/');
     if not DirectoryExists('/tmp/listaller/catalogue/cache') then CreateDir('/tmp/listaller/catalogue/cache');
   // ShowMessage(tmp[0]);
   if (pos('http://',LowerCase(tmp[0]))>0) then begin
    xHTTP.Clear;
    for i:=0 to SCForm.SWLLength-1 do
      SWList[i].UnButton.Enabled:=false;
     SCForm.CView.Enabled:=false;

     SCForm.current:=enr;
     SCForm.BitBtn2.Enabled:=true;
   if not xHTTP.HTTPMethod('GET',tmp[0]) then begin if SCForm.BitBtn2.Enabled then ShowMessage(StringReplace(strErrContactMan,'%h','http://listaller.nlinux.org',[rfReplaceAll]));
   SCForm.Label2.Caption:='Ready.';xHTTP.Free;tmp.Free;exit;end;

    if FileExists('/tmp/listaller/catalogue/cache/'+ExtractFileName(tmp[0])) then DeleteFile('/tmp/listaller/catalogue/cache/'+ExtractFileName(tmp[0]));
    xHTTP.Document.SaveToFile('/tmp/listaller/catalogue/cache/'+ExtractFileName(tmp[0]));
    xHTTP.Free;

   SCForm.Label2.Caption:=strInstalling;
   SCForm.BitBtn2.Enabled:=false;
   SCForm.current:=-1;
   Application.ProcessMessages;
  t:=TProcess.Create(nil);
  t.Options:=[poWaitOnExit];
  t.CommandLine := ExtractFilePath(Application.ExeName)+'listallgo '+'/tmp/listaller/catalogue/cache/'+ExtractFileName(tmp[0]);
  t.Execute;
  t.Free;
  end;
   tmp.Free;
   for i:=0 to SCForm.SWLLength-1 do
      SWList[i].UnButton.Enabled:=true;
      SCForm.CView.Enabled:=true;
   SCForm.pb1:=false;
  SCForm.Label2.Caption:=strReady;
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

function FindChildNode2(dn: TDOMNode; n: String): TDOMNode;
var i: Integer;
begin
Result:=nil;
for i:=0 to dn.ChildNodes.Count-1 do begin
if LowerCase(dn.ChildNodes.Item[i].NodeName)=LowerCase(n) then begin
Result:=dn.ChildNodes.Item[i];break;exit;end;
end;
end;

procedure TSCForm.BitBtn1Click(Sender: TObject);
begin
  close;
end;

procedure TSCForm.BitBtn2Click(Sender: TObject);
begin
  if current>-1 then
   if Application.MessageBox(PChar(strctDLAbort),PChar(strAbort+'?'),MB_YESNO)=IDYES then begin
   if not Assigned(SWList[current]) then ShowMessage('Oops!');
   BitBtn2.Enabled:=false;
   //SWList[current].xHTTP.Sock.StopFlag:=true;
   SWList[current].xHTTP.Abort;
   DLProgress.Position:=100;
   end;
end;

procedure TSCForm.ComboBox1Change(Sender: TObject);
begin
 if ComboBox1.ItemIndex= 0 then
 begin CatalogPath:=LiSrvPath+'catalogue/'; GetCatalog(); end else
 begin
  CatalogPath:=lsrv[ComboBox1.ItemIndex]; GetCatalog();end;
end;

procedure TSCForm.CViewClick(Sender: TObject);
begin
CView.Enabled:=false;
  OpenCatalog();
CView.Enabled:=true;
end;

procedure TSCForm.CViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 CView.Enabled:=false;
  OpenCatalog;
 CView.Enabled:=true;
end;

procedure TSCForm.GetCatalog;
begin
//Get catalogue info
    HTTP.HTTPMethod('GET', catalogpath+'contents.xml');
    if not DirectoryExists('/tmp/listaller') then CreateDir('/tmp/listaller/');
    if not DirectoryExists('/tmp/listaller/catalogue') then CreateDir('/tmp/listaller/catalogue/');
    HTTP.Document.SaveToFile('/tmp/listaller/catalogue/contents.xml');
    Label2.Caption:=strReady;
    OpenCatalog;
end;

procedure TSCForm.FormActivate(Sender: TObject);
var cnf: TIniFile;ls: TStringList;I: Integer;
begin
if fActiv then begin
  fActiv:=false;

  Label2.Caption:=strDownloadCTBase;
  SWLLength:=0;
  CView.Items[0].Selected:=true;
  HTTP := THTTPSend.Create;
  // HTTP.KeepAlive:=true;
  //Add Hook
  HTTP.Sock.OnStatus:=@HookSock;
  HTTP.UserAgent:='Listaller-GET';
  //Set HTTP settings
  cnf:=TInifile.Create(ConfigDir+'config.cnf');
    if cnf.ReadBool('Proxy','UseProxy',false) then begin
    HTTP.ProxyPort:=cnf.ReadString('Proxy','Port','');
    HTTP.ProxyHost:=cnf.ReadString('Proxy','Server','');
    end;
  cnf.Free;

  lsrv:=TStringList.Create;

  try
  //Get catalogue list
  HTTP.HTTPMethod('GET', LiSrvPath+'clist.srv');
  HTTP.Document.SaveToFile('/tmp/listaller/cservers.srv');
  ls:=TStringList.Create;
  ls.LoadFromFile('/tmp/listaller/cservers.srv');

  for I:=0 to ls.Count-1 do begin
    ComboBox1.Items.Add(copy(ls[i],1,pos(' ',ls[i])-1));
    lsrv.Add(copy(ls[i],pos(' ',ls[i]),length(ls[i])));
    end;
  ls.Free;
  except end;
  HTTP.Clear;

  GetCatalog();
  end;
end;

procedure TSCForm.HookSock(Sender: TObject; Reason: THookSocketReason;
const Value: string);
begin
Application.ProcessMessages;
//HTTP
if (Http.Document.Size>0) then begin
  DLProgress.Max:=HTTP.DownloadSize;
  DLProgress.Position:=HTTP.Document.Size;
 end;
end;

procedure TSCForm.OpenCatalog;
var xn    : TDOMNode;
    doc      : TXMLDocument;
    cdir,nid : String;
    i,k,cid      : Integer;
begin
//Read catalogue info
   ReadXMLFile(doc,'/tmp/listaller/catalogue/contents.xml');
//Clear List
CView.Enabled:=false;
for i:=0 to SWLLength-1 do
   SWList[i].Free;
   SWLLength:=0;

    case CView.Selected.Index of
     0: cdir:='all';
     1: cdir:='education';
     2: cdir:='office';
     3: cdir:='development';
     4: cdir:='graphic';
     5: cdir:='network';
     6: cdir:='games';
     7: cdir:='system';
     8: cdir:='multimedia';
     9: cdir:='other';
    end;
 Label2.Caption:=strOpenPage;
   if cdir<>'all' then begin
   xn:=doc.DocumentElement.FindNode(cdir);
   k:=0;
   MnProgress.Max:=xn.ChildNodes.Count+1;
    for i:=0 to xn.ChildNodes.Count-1 do begin
        k:=i+1;
        SetLength(SWList,k);
        SWLLength:=k;
        Dec(k);
        SWList[k]:=TCTLEntry.Create(SCForm);
        SWList[k].Parent:=ScrollBox1;
        SWList[k].id:=CView.Selected.Index;
        SWList[k].enr:=k;

        if xn.ChildNodes[i].Attributes.GetNamedItem('lname')<> nil then
        SWList[k].AppLabel.Caption:=xn.ChildNodes[i].Attributes.GetNamedItem('lname').NodeValue
        else
        SWList[k].AppLabel.Caption:=xn.ChildNodes[i].NodeName;

        SWList[k].VLabel.Visible:=false;

        SWList[k].DescLabel.Caption:=FindChildNode2(xn.ChildNodes[i],'sdesc').Attributes.GetNamedItem('std').NodeValue;
        if FindChildNode(FindChildNode2(xn.ChildNodes[i],'sdesc'),copy(GetEnvironmentVariable('LANG'), 1, 2))<>nil then
           SWList[k].DescLabel.Caption:=FindChildNode(FindChildNode2(xn.ChildNodes[i],'sdesc'),copy(GetEnvironmentVariable('LANG'), 1, 2)).NodeValue;
        if FindChildNode(xn.ChildNodes[i],'author')<>nil then
           SWList[k].MnLabel.Caption:=strAuthor+': '+FindChildNode(xn.ChildNodes[i],'author').NodeValue
        else SWList[k].MnLabel.Visible:=false;
           nid:=xn.ChildNodes[i].Attributes.GetNamedItem('idname').NodeValue;
        if not DirectoryExists('/tmp/listaller/catalogue/'+cdir) then CreateDir('/tmp/listaller/catalogue/'+cdir);
        if not DirectoryExists('/tmp/listaller/catalogue/'+cdir+'/icons/') then CreateDir('/tmp/listaller/catalogue/'+cdir+'/icons/');
        if not DirectoryExists('/tmp/listaller/catalogue/'+cdir+'/texts/') then CreateDir('/tmp/listaller/catalogue/'+cdir+'/texts/');
        SWList[k].rnm:=nid;

        if IsInList(LowerCase(nid),instLst) then SWList[k].UnButton.Enabled:=false;
      try
       if not FileExists('/tmp/listaller/catalogue/'+cdir+'/icons/'+nid+'.png') then begin
        HTTP.HTTPMethod('GET', catalogpath+cdir+'/icons/'+nid+'.png');
        HTTP.Document.SaveToFile('/tmp/listaller/catalogue/'+cdir+'/icons/'+nid+'.png');
       end;
       if not FileExists('/tmp/listaller/catalogue/'+cdir+'/texts/'+nid+'.txt') then begin
       HTTP.HTTPMethod('GET', catalogpath+cdir+'/texts/'+nid+'.txt');
       HTTP.Document.SaveToFile('/tmp/listaller/catalogue/'+cdir+'/texts/'+nid+'.txt');
       end;
       except end;

       try
        if FileExists('/tmp/listaller/catalogue/'+cdir+'/icons/'+nid+'.png') then
          SWList[k].SetImage('/tmp/listaller/catalogue/'+cdir+'/icons/'+nid+'.png');
       except end;

          Inc(k);
          MnProgress.Position:=k+1;
         Application.ProcessMessages;
end;
   end else begin
//Load all packages
   k:=0;

while cdir<>'other' do begin
if cdir='multimedia' then begin cdir:='other';cid:=9;end;
if cdir='system' then begin cdir:='multimedia';cid:=8;end;
if cdir='games' then begin cdir:='system';cid:=7;end;
if cdir='network' then begin cdir:='games';cid:=6;end;
if cdir='graphic' then begin cdir:='network';cid:=5;end;
if cdir='development' then begin cdir:='graphic';cid:=4;end;
if cdir='office' then begin cdir:='development';cid:=3;end;
if cdir='education' then begin cdir:='office';cid:=2;end;
if cdir='all' then begin cdir:='education';cid:=1;end;
 xn:=doc.DocumentElement.FindNode(cdir);
 if xn<>nil then begin
 MnProgress.Max:=xn.ChildNodes.Count+1;
    for i:=0 to xn.ChildNodes.Count-1 do begin
        k:=k+1;
        SetLength(SWList,k);
        SWLLength:=k;
        Dec(k);
        SWList[k]:=TCTLEntry.Create(nil);
        SWList[k].Parent:=ScrollBox1;
        SWList[k].id:=cid;
        SWList[k].enr:=k;
        if xn.ChildNodes[i].Attributes.GetNamedItem('lname')<> nil then
        SWList[k].AppLabel.Caption:=xn.ChildNodes[i].Attributes.GetNamedItem('lname').NodeValue
        else
        SWList[k].AppLabel.Caption:=xn.ChildNodes[i].NodeName;

        SWList[k].VLabel.Visible:=false;

        SWList[k].DescLabel.Caption:=FindChildNode2(xn.ChildNodes[i],'sdesc').Attributes.GetNamedItem('std').NodeValue;
       if FindChildNode(FindChildNode2(xn.ChildNodes[i],'sdesc'),copy(GetEnvironmentVariable('LANG'), 1, 2))<>nil then
           SWList[k].DescLabel.Caption:=FindChildNode(FindChildNode2(xn.ChildNodes[i],'sdesc'),copy(GetEnvironmentVariable('LANG'), 1, 2)).NodeValue;
        if FindChildNode(xn.ChildNodes[i],'author')<>nil then
           SWList[k].MnLabel.Caption:=strAuthor+': '+FindChildNode(xn.ChildNodes[i],'author').NodeValue
        else SWList[k].MnLabel.Visible:=false;
           nid:=xn.ChildNodes[i].Attributes.GetNamedItem('idname').NodeValue;
        if not DirectoryExists('/tmp/listaller/catalogue/'+cdir) then CreateDir('/tmp/listaller/catalogue/'+cdir);
        if not DirectoryExists('/tmp/listaller/catalogue/'+cdir+'/icons/') then CreateDir('/tmp/listaller/catalogue/'+cdir+'/icons/');
        if not DirectoryExists('/tmp/listaller/catalogue/'+cdir+'/texts/') then CreateDir('/tmp/listaller/catalogue/'+cdir+'/texts/');
        SWList[k].rnm:=nid;

        if IsInList(LowerCase(nid),instLst) then SWList[k].UnButton.Enabled:=false;
      try
       if not FileExists('/tmp/listaller/catalogue/'+cdir+'/icons/'+nid+'.png') then begin
        HTTP.Clear;
        HTTP.HTTPMethod('GET', catalogpath+cdir+'/icons/'+nid+'.png');
        HTTP.Document.SaveToFile('/tmp/listaller/catalogue/'+cdir+'/icons/'+nid+'.png');
       end;
       HTTP.Clear;
        HTTP.HTTPMethod('GET', catalogpath+cdir+'/texts/'+nid+'.txt');
        HTTP.Document.SaveToFile('/tmp/listaller/catalogue/'+cdir+'/texts/'+nid+'.txt');
       except end;

       try
        if FileExists('/tmp/listaller/catalogue/'+cdir+'/icons/'+nid+'.png') then
          SWList[k].SetImage('/tmp/listaller/catalogue/'+cdir+'/icons/'+nid+'.png');
       except end;

          Inc(k);
         Application.ProcessMessages;
         MnProgress.Position:=k+1;
end;
 end;
   end;
   end;
   ScrollBox1.Visible:=true;
   CView.Enabled:=true;
  Label2.Caption:=strReady;
end;

procedure TSCForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var i: Integer;
begin
  HTTP.Free;
  lsrv.Free;
  for i:=0 to SWLLength-1 do
    SWList[i].Free;
    SWLLength:=0;
    fActiv:=true;
end;

procedure TSCForm.FormCreate(Sender: TObject);
begin
 FillImageList(ImageList1);
  //Translation
  Label3.Caption:=strCategory;
  Label1.Caption:=strWInstallDl;
  CView.Items[0].Caption:=strAll;
  CView.Items[1].Caption:=strEducation;
  CView.Items[2].Caption:=strOffice;
  CView.Items[3].Caption:=strDevelopment;
  CView.Items[4].Caption:=strGraphic;
  CView.Items[5].Caption:=strNetwork;
  CView.Items[6].Caption:=strGames;
  CView.Items[7].Caption:=strSystem;
  CView.Items[8].Caption:=strMultimedia;
  CView.Items[9].Caption:=strOther;
  Memo1.Lines.Clear;
  Memo1.Lines.Add(strNoInfo);
  BitBtn1.Caption:=strClose;
  BitBtn2.Caption:=strAbort;
  LoadStockPixmap(STOCK_CLOSE,ICON_SIZE_BUTTON,BitBtn1.Glyph);
end;

procedure TSCForm.FormDestroy(Sender: TObject);
var p: TProcess;
begin
 p:=TProcess.Create(nil);
 p.Options:=[poUsePipes,poWaitOnExit];
 p.CommandLine:='chmod 777 -R '+'/tmp/listaller/catalogue/';
 p.Execute;
 p.Free;
end;

procedure TSCForm.HQBoxChange(Sender: TObject);
begin
  if not HQBox.Checked then begin
     Memo1.Visible:=false; Splitter1.Visible:=false; end else begin
     Splitter1.Enabled:=true;
     Memo1.Visible:=true;
     end;
end;

initialization
  {$I swcatalog.lrs}

end.

