{ common.pas
  Copyright (C) Listaller Project 2008-2009

  common.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  common.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** This unit contains some various functions which are used global
unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, FileUtil, ExtCtrls, process, Buttons, LCLType, RegExpr,
  {$IFDEF LCLGTK2}
  gtk2, gtkint, gtkdef, gdkpixbuf, gtkproc, gdk2,
  {$ENDIF} trstrings, distri, IniFiles, DOS;

const
  //** Version of the Listaller applicationset
  LiVersion='0.2.30a-dev';
var
  //** True if Listaller is in testmode
  Testmode: Boolean=false;
type

//** Listaller package types
TListallerPackageType = (lptLinstall, lptDLink, lptContainer);

//** One entry of Listaller's visual software lists
TListEntry = class(TPanel) //Helper class / a software entry
protected
AppLabel: TLabel;
DescLabel: TLabel;
Vlabel: TLabel;
MnLabel: TLabel;
Graphic: TImage;
ID: Integer;
sID: String;
private
 procedure SetAppName(s: String);
 function GetAppName: String;
 procedure SetAppDesc(s: String);
 function GetAppDesc: String;
 procedure SetAppMaintainer(s: String);
 function GetAppMaintainer: String;
 procedure SetAppVersion(s: String);
 function GetAppVersion: String;
public
UnButton: TBitBtn;
//** The application's ID string
property aID:Integer read ID write ID;
//** Name of the application
property AppName: String read GetAppName write SetAppName;
//** Description of the application
property AppDesc: String read GetAppDesc write SetAppDesc;
//** Name of the maintainer(s)
property AppMn: String read GetAppMaintainer write SetAppMaintainer;
//** Version of the application
property AppVersion: String read GetAppVersion write SetAppVersion;
//** Software removal ID
property srID: String read sID write sID;
//** Set an image for the entry
procedure SetImage(AImage: String);
//** Constructor
constructor Create(AOwner: TComponent); override;
//** Destructor
destructor Destroy; override;
end;

//** Remove modifiers from a string @returns Cleaned string
function  DeleteModifiers(s: String): String;
//** Replaces placeholders (like $INSt or $APP) with their current paths @returns Final path as string
function  SyblToPath(s: String): String;
//** Removes every symbol or replace it an simpla dummy path @returns Cleaned string
function  SyblToX(s: String): String;
//** Check if file is a shared one @returns States as bool
function  IsSharedFile(s: String): Boolean;
//** Creates Listaller's config dir @returns Current config dir
function  ConfigDir: String;
//** Get current data file (check /usr/share and current dir)
function  GetDataFile(s: String): String;
//** Executes a command-line application @returns The application's last output string
function  CmdResult(cmd:String):String;
//** Executes a command-line application @returns The application's exit code
function  CmdResultCode(cmd:String):Integer;
{** Executes a command-line application
    @param cmd Command that should be executed
    @param Result TStringList to recive the output}
procedure CmdResultList(cmd:String;Result: TStringList);
//** Executes a command-line application @returns The final application output string (without breaks in line and invisible codes)
function  CmdFinResult(cmd: String): String;
//** Advanced file copy method @returns Success of the command
function  FileCopy(source,dest: String): Boolean;
//** Loads a stock icon (native on GTK2, if Qt4 is used aditional images are needed) @returns Handle to th bitmap
procedure LoadStockPixmap(StockId: PChar; IconSize: integer;bmp: TBitmap);
//** Get server name from an url @returns The server name
function  GetServerName(url:string):string;
//** Path on an server (from an url) @returns The path on the server
function  GetServerPath(url:string):string;
{** Fast check if entry is in a list
    @param nm Name of the entry that has to be checked
    @param list The string list that has to be searched}
function  IsInList(nm: String;list: TStringList): Boolean;
//** Shows pkMon if option is set in preferences
procedure ShowPKMon();
//** Reads the current system architecture @returns Detected architecture as string
function GetSystemArchitecture: String;
{** Executes an application as root using an graphical input prompt
    @param cmd Command line string to execute
    @param comment Description of the action the user is doing (why are root-rights needed?
    @param icon Path to an icon for the operation
    @param optn Set of TProcessOption to apply on the process object}
function  ExecuteAsRoot(cmd: String;comment: String; icon: String;optn: TProcessOptions=[]): Boolean;
{** Get dependencies of an executable
    @param f Name of the binary file
    @param lst StringList to recieve the output
    @returns Success of operation}
function GetLibDepends(f: String; lst: TStringList): Boolean;
//** Get current date as string
function GetDateAsString: String;


const
//Stock constants
 STOCK_QUIT='stock-quit';
 STOCK_GO_FORWARD='stock-go-forward';
 STOCK_GO_BACK='stock-go-back';
 STOCK_DIALOG_WARNING='stock-dialog-warning';
 STOCK_CLOSE='stock-close';
 STOCK_DELETE='stock-delete';
 STOCK_EXECUTE='stock-execute';
 STOCK_APPLY='stock-apply';
 STOCK_DIALOG_INFO='stock-dialog-info';
 STOCK_REFRESH='stock-refresh';
 STOCK_OPEN='stock-open';
 STOCK_PROJECT_OPEN='stock-project-open';
 ICON_SIZE_BUTTON=4;
 ICON_SIZE_SMALL_TOOLBAR=2;
 ICON_SIZE_LARGE_TOOLBAR=3;
 ICON_SIZE_MENU=1;
 //
 //** Directory with KDE4 icons
 KDE_ICON_DIR='/usr/share/icons/default.kde4/';
implementation

procedure LoadStockPixmap(StockId: PChar; IconSize: integer; bmp: TBitmap);
{$IFDEF LCLGTK2}
function Gtk2LoadStockPixmap(StockId: PChar; IconSize: integer): HBitmap;
var
  StockWindow: PGtkWidget;
  Pixmap: PGDIObject;
  Pixbuf: PGDKPixbuf;
begin
  Result := 0;
  // If not a default icon then stop
  if gtk_icon_factory_lookup_default(StockId) = nil then Exit;
  StockWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  Pixbuf := gtk_widget_render_icon(StockWindow, StockId, IconSize, nil);
  gtk_widget_destroy(StockWindow);
  // Check if icon was assigned
  if PtrUInt(Pixbuf) = 0 then Exit;
  Pixmap := GtkWidgetSet.NewGDIObject(gdiBitmap);
  with Pixmap^ do
  begin
    GDIBitmapType := gbPixmap;
    visual := gdk_visual_get_system();
    gdk_visual_ref(visual);
    colormap := gdk_colormap_get_system();
    gdk_colormap_ref(colormap);
    gdk_pixbuf_render_pixmap_and_mask(Pixbuf, GDIPixmapObject.Image,
      GDIPixmapObject.Mask, 192);
  end;
  gdk_pixbuf_unref(Pixbuf);
  Result := HBitmap(PtrUInt(Pixmap));
end;
var cicon: String;BH: HBitmap;
begin
{$ENDIF}
{$IFDEF LCLGTK2}
 if StockId = STOCK_PROJECT_OPEN then StockId:=STOCK_OPEN;
 cicon:=StringReplace(StockId,'stock','gtk',[rfReplaceAll]);
 BH:=Gtk2LoadStockPixmap(PChar(cicon),IconSize);
 if BH <> 0 then
    bmp.Handle:=BH;
{$ELSE}
var cicon: String;pic: TPicture;s: String;
begin
if not DirectoryExists(KDE_ICON_DIR+'22x22/') then
 if not DirectoryExists('/usr/share/icons/default.kde/22x22') then
  if not DirectoryExists('/usr/share/icons/oxygen/22x22') then
   if not DirectoryExists('/usr/share/icons/hicolor/22x22') then exit;
 if StockId = STOCK_QUIT then cicon:='process-stop.png';
 if StockId = STOCK_GO_FORWARD then cicon:='arrow-right.png';
 if StockId = STOCK_GO_BACK then cicon:='arrow-left.png';
 if StockId = STOCK_DIALOG_WARNING then cicon:='../status/dialog-warning.png';
 if StockId = STOCK_CLOSE then cicon:='window-close.png';
 if StockId = STOCK_DELETE then cicon:='edit-delete.png';
 if StockId = STOCK_EXECUTE then cicon:='fork.png';
 if StockId = STOCK_APPLY then cicon:='dialog-ok-apply.png';
 if StockId = STOCK_DIALOG_INFO then cicon:='help-hint.png';
 if StockId = STOCK_REFRESH then cicon:='view-refresh.png';
 if StockId = STOCK_PROJECT_OPEN then cicon:='project-open.png';
 if StockId = STOCK_OPEN then cicon:='../places/folder.png';

 if IconSize=4 then
  s:=KDE_ICON_DIR+'16x16/actions/';
 if IconSize=3 then
  s:=KDE_ICON_DIR+'64x64/actions/';
 if IconSize=2 then
  s:=KDE_ICON_DIR+'22x22/actions/';
 if IconSize=1 then
  s:=KDE_ICON_DIR+'32x32/actions/';
 if not FileExists(s) then exit;
 pic:=TPicture.Create;
 pic.LoadFromFile(s+cicon);
 bmp.Clear;
 bmp.LoadFromBitmapHandles(pic.Bitmap.Handle,pic.Bitmap.MaskHandle);
 pic.Free;
{$ENDIF}
end;


function GetServerName(url:string):string;
var p1,p2 : integer;
begin
url := StringReplace(url,'ftp://','',[rfReplaceAll]);
url := StringReplace(url,'http://','',[rfReplaceAll]);
p1 := 1;
p2 := Pos('/',url);
//
Result := Copy(url,P1,(P2-1));
end;

function GetServerPath(url:string):string;
var p1,p2 : integer;
begin
url := StringReplace(url,'http://','',[rfReplaceAll]);
url := StringReplace(url,'ftp://','',[rfReplaceAll]);
url := StringReplace(url,'www.','',[rfReplaceAll]);
p1 := Pos('/',url);
p2 := Length(url);
Result := ExtractFilePath(Copy(url,P1,P2));
end;

function DeleteModifiers(s: String): String;
var h: String;
begin
h:=SysUtils.StringReplace(s,' <s>','',[rfReplaceAll]);
h:=ReplaceRegExpr(' <chmod:([0-7]{3})>', h, '', false);
h:=ReplaceRegExpr(' <([a-zA-Z_]{4,})-only>', h, '', false);
h:=SysUtils.StringReplace(h,' <mime>','',[rfReplaceAll]);
h:=SysUtils.StringReplace(h,'>','',[rfReplaceAll]);
Result:=h;
end;

function GetDataFile(s: String): String;
begin
if (FileExists(ExtractFilePath(Application.ExeName)+s))
or (DirectoryExists(ExtractFilePath(Application.ExeName)+s)) then
Result:=ExtractFilePath(Application.ExeName)+s
else
Result:='/usr/share/listaller/'+s;
end;

function GetSystemArchitecture: String;
var p: TProcess;x: TStringList;s: String;
begin
 p:=TProcess.Create(nil);
 p.Options:=[poUsePipes, poWaitOnExit];
 p.CommandLine:='uname -m';
 p.Execute;
 x:=TStringList.Create;
 x.LoadFromStream(p.OutPut);
 s:=x[0];
 x.Free;
 p.Free;

if (s='i686')
or (s='i586')
or (s='i486')
then s:='i386';
Result:=s;
end;

function GetLibDepends(f: String; lst: TStringList): Boolean;
var p: TProcess;s: TStringList;i: Integer;
begin
  p:=TProcess.Create(nil);
  p.Options:=[poUsePipes,poWaitOnExit];
  p.CommandLine:='ldd '+f;
  p.Execute;
  Result:=true;
  if p.ExitStatus>0 then
  begin
   Result:=false;
   p.Free;
   exit;
  end;
  s:=TStringList.Create;
  s.LoadFromStream(p.Output);
  p.Free;
  for i:=0 to s.Count-1 do
   if pos('=>',s[i])>0 then
   begin
    lst.Add(copy(s[i],2,pos('=',s[i])-2));
   end else
    lst.Add(copy(s[i],2,pos('(',s[i])-2));
  s.Free;
end;

function IsSharedFile(s: String): Boolean;
begin
Result:=pos(' <s>',s)>0;
end;

function GetXHome: String;
begin
 if TestMode then
 begin
  Result:='/tmp/litest';
  CreateDir(Result);
  end else Result:=GetEnvironmentVariable('HOME');
end;

function SyblToPath(s: String): String;
var t: TProcess;n: String;x: TStringList;
begin
s:=StringReplace(s,'$HOME',GetEnvironmentVariable('HOME'),[rfReplaceAll]);

t:=TProcess.Create(nil);
 t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:='uname -m';
 t.Execute;
 x:=TStringList.Create;
 x.LoadFromStream(t.OutPut);
 n:=x[0];
 x.Free;
 t.Free;

if IsRoot then
begin
s:=StringReplace(s,'$INST','/usr/appfiles',[rfReplaceAll]);
s:=StringReplace(s,'$INST-X','/usr/share',[rfReplaceAll]);
s:=StringReplace(s,'$OPT','/opt',[rfReplaceAll]);
s:=StringReplace(s,'$BIN','/usr/bin',[rfReplaceAll]);

 if LowerCase(n)='x86_64' then
  s:=StringReplace(s,'$LIB','/usr/lib64',[rfReplaceAll])
 else
  s:=StringReplace(s,'$LIB','/usr/lib',[rfReplaceAll]);
s:=StringReplace(s,'$APP','/usr/share/applications',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-16','/usr/share/icons/hicolor/16x16/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-24','/usr/share/icons/hicolor/24x24/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-32','/usr/share/icons/hicolor/32x32/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-48','/usr/share/icons/hicolor/48x48/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-64','/usr/share/icons/hicolor/64x64/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-128','/usr/share/icons/hicolor/128x128/apps',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-256','/usr/share/icons/hicolor/256x256/apps',[rfReplaceAll]);
s:=StringReplace(s,'$PIX','/usr/share/pixmaps',[rfReplaceAll]);
end else
begin
if not DirectoryExists(GetXHome+'/applications/') then CreateDir(GetXHome+'/applications/');
if not DirectoryExists(GetXHome+'/applications/files') then CreateDir(GetXHome+'/applications/files');
if not DirectoryExists(GetXHome+'/applications/files/icons') then CreateDir(GetXHome+'/applications/files/icons');
//Iconpaths
if not DirectoryExists(GetXHome+'/applications/files/icons/16x16')  then CreateDir(GetXHome+'/applications/files/icons/16x16');
if not DirectoryExists(GetXHome+'/applications/files/icons/24x24')  then CreateDir(GetXHome+'/applications/files/icons/24x24');
if not DirectoryExists(GetXHome+'/applications/files/icons/32x32')  then CreateDir(GetXHome+'/applications/files/icons/32x32');
if not DirectoryExists(GetXHome+'/applications/files/icons/48x48')  then CreateDir(GetXHome+'/applications/files/icons/48x48');
if not DirectoryExists(GetXHome+'/applications/files/icons/64x64')  then CreateDir(GetXHome+'/applications/files/icons/64x64');
if not DirectoryExists(GetXHome+'/applications/files/icons/128x128')then CreateDir(GetXHome+'/applications/files/icons/128x128');
if not DirectoryExists(GetXHome+'/applications/files/icons/256x256')then CreateDir(GetXHome+'/applications/files/icons/256x256');
if not DirectoryExists(GetXHome+'/applications/files/icons/common')then CreateDir(GetXHome+'/applications/files/icons/common');
if LowerCase(n)='x86_64' then
begin
 if not DirectoryExists(GetXHome+'/applications/files/lib64')then CreateDir(GetXHome+'/applications/files/lib64');
end else
 if not DirectoryExists(GetXHome+'/applications/files/lib')then CreateDir(GetXHome+'/applications/files/lib');
//
s:=StringReplace(s,'$BIN',GetXHome+'/applications/files/binary',[rfReplaceAll]);
if LowerCase(n)='x86_64' then
  s:=StringReplace(s,'$LIB',GetXHome+'/applications/files/lib64',[rfReplaceAll])
 else
  s:=StringReplace(s,'$LIB',GetXHome+'/applications/files/lib',[rfReplaceAll]);

s:=StringReplace(s,'$INST',GetXHome+'/applications/files',[rfReplaceAll]);
s:=StringReplace(s,'$INST-X',GetXHome+'/applications/files',[rfReplaceAll]);
s:=StringReplace(s,'$OPT',GetXHome+'/applications/files',[rfReplaceAll]);
s:=StringReplace(s,'$APP',GetXHome+'/applications',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-16',GetXHome+'/applications/files/icons/16x16',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-24',GetXHome+'/applications/files/icons/24x24',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-32',GetXHome+'/applications/files/icons/32x32',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-48',GetXHome+'/applications/files/icons/48x48',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-64',GetXHome+'/applications/files/icons/64x64',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-128',GetXHome+'/applications/files/icons/128x128',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-256',GetXHome+'/applications/files/icons/256x256',[rfReplaceAll]);
s:=StringReplace(s,'$PIX',GetXHome+'/applications/files/icons/common',[rfReplaceAll]);
end;
Result:=s;
end;

function SyblToX(s: String): String;
begin
s:=StringReplace(s,'$INST','',[rfReplaceAll]);
s:=StringReplace(s,'$INST-X','',[rfReplaceAll]);
s:=StringReplace(s,'$OPT','/opt',[rfReplaceAll]);
s:=StringReplace(s,'$APP','/app',[rfReplaceAll]);
s:=StringReplace(s,'$HOME','/hdir',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-16','/icon16',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-24','/icon24',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-32','/icon32',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-48','/icon48',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-64','/icon64',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-128','/icon128',[rfReplaceAll]);
s:=StringReplace(s,'$ICON-256','/icon265',[rfReplaceAll]);
s:=StringReplace(s,'$PIX','/icon',[rfReplaceAll]);
s:=StringReplace(s,'$LIB','/lib',[rfReplaceAll]);
s:=StringReplace(s,'$LIB','/binary',[rfReplaceAll]);
Result:=s;
end;

function ConfigDir: String;
var h: String;
begin
h:=getenvironmentvariable('HOME');
h:=h+'/.config';
if not DirectoryExists(h) then CreateDir(h);
h:=h+'/Listaller';
if not DirectoryExists(h) then CreateDir(h);
Result:=h+'/';
end;

function CmdResult(cmd:String):String;
var t:TProcess;
s:TStringList;
begin
 Result:='';
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes, poWaitOnExit];
 t.CommandLine:=cmd;
 try
  t.Execute;
  s:=tstringlist.Create;
  try
   s.LoadFromStream(t.Output);
   if s.Count<= 0 then Result:='err' else
   Result:=s[0];
  finally
  s.free;
  end;
 finally
 t.Free;
 end;
end;

procedure ShowPKMon();
var cnf: TIniFile;t: TProcess;
begin
//Check if PackageKit checkmode is enabled:
cnf:=TIniFile.Create(ConfigDir+'config.cnf');
  if cnf.ReadBool('MainConf','ShowPkMon',false) then
  begin
   t:=TProcess.Create(nil);
   t.CommandLine:='pkmon';
   t.Options:=[poNewConsole];
   t.Execute;
  end;
cnf.Free;
end;

function CmdFinResult(cmd:String):String;
var t:TProcess;
Buffer: string;
BytesAvailable: DWord;
BytesRead:LongInt;
begin
 Result:='';
 buffer:='';
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes];
 t.CommandLine:=cmd;
 try
  t.Execute;
  while t.Running do
  begin
      BytesAvailable := t.Output.NumBytesAvailable;
      BytesRead := 0;
      while BytesAvailable>0 do
       begin
        SetLength(Buffer, BytesAvailable);
        BytesRead := t.OutPut.Read(Buffer[1], BytesAvailable);
        if (pos(#13,Buffer)>0)or(pos(#26,Buffer)>0)or(Pos(#10,Buffer)>0)then Result:='';
        Result := Result + copy(Buffer,1, BytesRead);
        BytesAvailable := t.OutPut.NumBytesAvailable;
      end;
  end;
 finally
 t.Free;
 end;
end;

function CmdResultCode(cmd:String):Integer;
var t:TProcess;
begin
 Result:=0;
 t:=tprocess.create(nil);
 t.Options:=[poUsePipes,poWaitonexit];
 t.CommandLine:=cmd;
 try
  t.Execute;
   Result:=t.ExitStatus;
 finally
 t.Free;
 end;
end;

function GetDateAsString: String;
var
  Year,Month,Day,WDay : word;
begin
  GetDate(Year,Month,Day,WDay);
  Result:=IntToStr(Day)+'.'+IntToStr(Month)+'.'+IntToStr(Year) ;
end;

function ExecuteAsRoot(cmd: String;comment: String; icon: String;optn: TProcessOptions=[]): Boolean;
var p: TProcess; DInfo: TDistroInfo;
begin
DInfo:=GetDistro;
p:=TProcess.Create(nil);
if DInfo.DBase='KDE' then
 if FileExists('/usr/bin/kdesu') then
  p.CommandLine := 'kdesu -d --comment "'+comment+'" -i '+icon+' '+cmd
 else
 if FileExists('/usr/bin/kdesudo') then
  p.CommandLine := 'kdesudo -d --comment "'+comment+'" -i '+icon+' '+cmd
  else
 if DInfo.DName='Fedora' then
  //Fedora uses Consolehelper to run apps as root. So we use "beesu" to make CH work for Listaller
  p.CommandLine := 'beesu -l '+cmd
 else
   if FileExists('/usr/bin/gksudo') then
    p.CommandLine := 'gksudo --message "'+comment+'" '+cmd
    else
   if FileExists('/usr/bin/gksu') then
    p.CommandLine := 'gksu --message "'+comment+'" '+cmd
    else
   if FileExists('/usr/bin/gnomesu') then
    p.CommandLine := 'gnomesu '+cmd
   else
   begin
    ShowMessage('Unable to execute the application as root.'#13'Please do this manually!');
    p.Free;
    Result:=false;
    exit;
   end;
    p.Options:=optn;
    p.Execute;
    Result:=true;
    if p.ExitStatus>0 then Result:=false;
    p.Free;
end;

procedure CmdResultList(cmd:String;Result: TStringList);
var t:TProcess;
s:TStringList;
begin
 t:=tprocess.create(nil);
 t.CommandLine:=cmd;
 t.Options:=[poUsePipes,poWaitonexit];
 try
  t.Execute;
  s:=tstringlist.Create;
  try
   s.LoadFromStream(t.Output);
   if t.ExitStatus<=0 then
   Result.Assign(s)
   else Result.Clear;

  finally
  s.free;
  end;
 finally
 t.Free;
 end;
end;

function IsInList(nm: String;list: TStringList): Boolean;
begin
Result:=list.IndexOf(nm)>-1;
end;

function FileCopy(source,dest: String): Boolean;
var
 fSrc,fDst,len: Integer;
 ct,units,size: Longint;
 buffer: packed array [0..2047] of Byte;
begin
 ct:=0;
 Result := False; { Assume that it WONT work }
 if source <> dest then
 begin
   fSrc := FileOpen(source,fmOpenRead);
   if fSrc >= 0 then
   begin
     size := FileSeek(fSrc,0,2);
     units:=size div 2048;
     FileSeek(fSrc,0,0);
     fDst := FileCreate(dest);
     if fDst >= 0 then
     begin
       while size > 0 do
       begin
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

{ TListEntry }
constructor TListEntry.Create(AOwner: TComponent);
begin
inherited Create(AOwner);
id:=-1;
self.Align:=alTop;
self.Height:=80;
self.BevelOuter:=BVNone;
self.BevelInner:=bvLowered;

AppLabel:=TLabel.Create(nil);
with AppLabel do
begin
Parent:=Self;
AutoSize:=true;
Caption:='<appname>';
Font.Size:=16;
Anchors:=[akLeft,akTop];
Top:=10;
Left:=84;
end;

mnLabel:=TLabel.Create(nil);
with mnLabel do
begin
Parent:=Self;
AutoSize:=true;
Anchors:=[akLeft,akTop];
Top:=42;
Left:=96;
Caption:='<creator>';
end;

DescLabel:=TLabel.Create(nil);
with DescLabel do
begin
Parent:=Self;
AutoSize:=true;
Anchors:=[akLeft,akTop];
Top:=30;
Left:=90;
Caption:='<description>';
end;

vLabel:=TLabel.Create(nil);
with vLabel do
begin
Parent:=Self;
AutoSize:=true;
Anchors:=[akBottom,akRight];
Top:=self.Height-60;
Left:=self.Width-140;
Caption:='';
end;

Graphic:=TImage.Create(nil);
with Graphic do
begin
Parent:=self;
Width:=64;
Height:=64;
Top:=8;
Left:=8;
Center:=true;
Anchors:=[akLeft];
Stretch:=true;
Proportional:=true;
Picture.LoadFromFile(GetDataFile('graphics/spackage.png'));
end;

UnButton:=TBitBtn.Create(nil);
with UnButton do
begin
Parent:=self;
Height:=30;
Width:=128;
Caption:=strUninstall;
Anchors:=[akBottom,akRight];
Top:=self.Height-46;
Left:=self.Width-140;
LoadStockPixmap(STOCK_DELETE,ICON_SIZE_BUTTON,Glyph);
end;
end;

destructor TListEntry.Destroy;
begin
AppLabel.Free;
UnButton.free;
Graphic.free;
mnLabel.Free;
DescLabel.Free;
vLabel.Free;
inherited Destroy;
end;

procedure TListEntry.SetAppName(s: String);
begin
 AppLabel.Caption:=s;
end;

function TListEntry.GetAppName: String;
begin
 Result:=AppLabel.Caption;
end;

procedure TListEntry.SetAppDesc(s: String);
begin
 DescLabel.Caption:=s;
end;

function TListEntry.GetAppDesc: String;
begin
 Result:=DescLabel.Caption;
end;

procedure TListEntry.SetAppMaintainer(s: String);
begin
 MNLabel.Caption:=s;
end;

function TListEntry.GetAppMaintainer: String;
begin
 Result:=MNLabel.Caption;
end;

procedure TListEntry.SetAppVersion(s: String);
begin
 VLabel.Caption:=s;
end;

function TListEntry.GetAppVersion: String;
begin
 Result:=VLabel.Caption;
end;

procedure TListEntry.SetImage(AImage: String);
begin
Graphic.Picture.LoadFromFile(AImage);
end;

end.

