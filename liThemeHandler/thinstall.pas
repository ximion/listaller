{ thinstall.pas
  Copyright (C) Listaller Project 2008

  thinstall.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  thinstall.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.

 -> Main unit of Listaller's desktop-theme manager/installer
}
unit thinstall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  utilities, Menus, ComCtrls, Buttons, distri, ExtCtrls, IniFiles, xtypefm,
  lctheme, StdCtrls, LCLType, Process;

type

  { TisFrm }

  TisFrm = class(TForm)
    headImage: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    HelpMItem: TMenuItem;
    PageControl1: TPageControl;
    Designs: TTabSheet;
    Iconthemes: TTabSheet;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    ScrollBox4: TScrollBox;
    Screensavers: TTabSheet;
    Wallpapers: TTabSheet;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure IconthemesShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure ScreensaversShow(Sender: TObject);
    procedure WallpapersShow(Sender: TObject);
  private
    { private declarations }
     DList: Array of TListEntry;
     IList: Array of TListEntry;
     WList: Array of TListEntry;
     SList: Array of TListEntry;
     DLLength, ILLength, WLLength, SLLength: Integer;
     procedure UninstallTheme(Sender: TObject);
     procedure UninstallWallpaper(Sender: TObject);
     procedure UninstallScreensaver(Sender: TObject);
  public
    { public declarations }
  end; 

var
  isFrm: TisFrm;

implementation

{ TisFrm }

procedure TisFrm.UninstallTheme(Sender: TObject);
begin
if Application.MessageBox(PChar(StringReplace(strRealUninstQ,'%a',DList[(Sender as TBitBtn).Tag].AppName,[rfReplaceAll])),PChar(strRmPkgQ),MB_YESNO) = IDYES then
if ExtractFileExt(DList[(Sender as TBitBtn).Tag].srmId)='' then
DeleteDirectory(DList[(Sender as TBitBtn).Tag].srmId,false)
else
DeleteFile(DList[(Sender as TBitBtn).Tag].srmId);
end;

procedure TisFrm.UninstallWallpaper(Sender: TObject);
var t: TProcess;
begin
if Application.MessageBox(PChar(StringReplace(strRealUninstQ,'%a',DList[(Sender as TBitBtn).Tag].AppName,[rfReplaceAll])),PChar(strRmPkgQ),MB_YESNO) = IDYES then
if ExtractFileExt(DList[(Sender as TBitBtn).Tag].srmId)='' then
DeleteDirectory(DList[(Sender as TBitBtn).Tag].srmId,false)
else
if GetDistro.Desktop='GNOME' then begin
if not IsRoot then begin
       t:=TProcess.Create(nil);
         if GetDistro.DName='Ubuntu' then
            t.CommandLine := 'gksudo rm '+DList[(Sender as TBitBtn).Tag].srmId
         else
          t.CommandLine := 'gnomesu rm '+DList[(Sender as TBitBtn).Tag].srmId;
        t.Options:=[];
        t.Execute;
        t.Free;
   end;
 end else
DeleteFile(DList[(Sender as TBitBtn).Tag].srmId);
end;

procedure TisFrm.UninstallScreensaver(Sender: TObject);
begin
//Code follows later
end;

procedure TisFrm.FormActivate(Sender: TObject);
var lst: TStringList;i,k: Integer;SR: TSearchRec;
    cnf: TIniFile;dir: String;
begin
if DLLength<1 then begin
 lst:=TStringList.Create;

 if IsRoot then begin
 dir:='/usr/share/themes/';
    if FindFirst(dir+'*', faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory = faDirectory then
        if (SR.Name<>' ') and (SR.Name <> '.') and (SR.name<>'..')
        and (FileExists(dir+SR.Name+'/index.theme'))then
          lst.Add(SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  end else begin
    dir:=GetEnvironmentVariable('HOME')+'/.themes/';
    if FindFirst(dir+'*', faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory = faDirectory then
        if (SR.Name<>' ') and (SR.Name <> '.') and (SR.name<>'..')
        and (FileExists(dir+SR.Name+'/index.theme'))then
          lst.Add(SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
   end;

 for i:=0 to lst.Count-1 do begin
     k:=DLLength+1;
     SetLength(DList,k);
     DLLength:=k;
     Dec(k);
     DList[k]:=TListEntry.Create(isFrm);
     DList[k].Parent:=ScrollBox1;
     DList[k].UnButton.Tag:=k;
     DList[k].UnButton.OnClick:=@UninstallTheme;
     DList[k].srmId:=dir+lst[i];

     cnf:=TIniFile.Create(dir+lst[i]+'/index.theme');
     DList[k].AppName:=cnf.ReadString('Desktop Entry','Name',lst[i]);
     DList[k].AppDesc:=cnf.ReadString('Desktop Entry','Comment['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','');
     if DList[k].AppDesc='' then
     DList[k].AppDesc:=cnf.ReadString('Desktop Entry','Comment','');

     DList[k].AppVersion:='';
     if DList[k].AppDesc='' then
     DList[k].AppDesc:='';
     DList[k].AppMn:='';

     DList[k].SetPositions;
    Inc(k);
    Application.ProcessMessages;
    end;
    lst.Free;
  end;
end;

procedure TisFrm.FormCreate(Sender: TObject);
var mdf: TimdFrm;
begin
 if not IsRoot then begin
   mdf:=TimdFrm.Create(self);
   with mdf do begin
    Label13.Visible:=false;
    Image1.Visible:=false;
    btnTest.Visible:=false;
    Caption:=strSelMode;
    btnInstallAll.Caption:=strDispSysDsgn;
    btnHome.Caption:=strDispUsrDsgn;
    ShowModal;
   end;
   mdf.Free;
 end;
  headImage.Picture.LoadFromFile(GetDataFile('graphics/ligraphic-header.png'));
end;

procedure TisFrm.FormDestroy(Sender: TObject);
var i: Integer;
begin
 for i:=0 to WLLength-1 do WList[i].Free;
 for i:=0 to ILLength-1 do IList[i].Free;
 for i:=0 to DLLength-1 do DList[i].Free;
 for i:=0 to SLLength-1 do SList[i].Free;
end;

procedure TisFrm.FormResize(Sender: TObject);
var i: Integer;
begin
case PageControl1.TabIndex of
0: for i:=0 to DLLength-1 do DList[i].SetPositions;
1: for i:=0 to ILLength-1 do IList[i].SetPositions;
2: for i:=0 to WLLength-1 do WList[i].SetPositions;
3: for i:=0 to SLLength-1 do SList[i].SetPositions;
end;
end;

procedure TisFrm.IconthemesShow(Sender: TObject);
var lst: TStringList;i,k: Integer;SR: TSearchRec;
     dir: String;
begin
if ILLength<1 then begin
 lst:=TStringList.Create;

 if IsRoot then begin
   dir:='/usr/share/icons/';
    if FindFirst(dir+'*', faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory = faDirectory then
        if (SR.Name<>' ') and (SR.Name <> '.') and (SR.name<>'..')
        and (SR.Name<>'hicolor') and (SR.Name<>'locolor') and
        (not DirectoryExists(dir+SR.Name+'/cursors')) then
          lst.Add(SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
 end else begin
  dir:=GetEnvironmentVariable('HOME')+'/.icons/';
    if FindFirst(dir+'*', faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory = faDirectory then
        if (SR.Name<>' ') and (SR.Name <> '.') and (SR.name<>'..')
        and (SR.Name<>'hicolor') and (SR.Name<>'locolor') and
        (not DirectoryExists(dir+SR.Name+'/cursors')) then
          lst.Add(SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
 end;


 for i:=0 to lst.Count-1 do begin
     k:=ILLength+1;
     SetLength(IList,k);
     ILLength:=k;
     Dec(k);
     IList[k]:=TListEntry.Create(isFrm);
     IList[k].Parent:=ScrollBox2;
     IList[k].UnButton.Tag:=k;
     IList[k].UnButton.OnClick:=@UninstallTheme;
     IList[k].srmId:=lst[i];

     IList[k].AppName:=lst[i];

     IList[k].AppVersion:='';;
     IList[k].AppDesc:='';
     IList[k].AppMN:='';

     try
     if FileExists(dir+lst[i]+'/64x64/places/folder.png') then
     IList[k].SetImage(dir+lst[i]+'/64x64/places/folder.png')
     else
     if FileExists(dir+lst[i]+'/48x48/places/folder.png') then
     IList[k].SetImage(dir+lst[i]+'/48x48/places/folder.png')
     else
     if FileExists(dir+lst[i]+'/32x32/places/folder.png') then
     IList[k].SetImage(dir+lst[i]+'/32x32/places/folder.png')
     else
     if FileExists(dir+lst[i]+'/24x24/places/folder.png') then
     IList[k].SetImage(dir+lst[i]+'/24x24/places/folder.png')
     else
     if FileExists(dir+lst[i]+'/48x48/categories/applications-accessories.png') then
     IList[k].SetImage(dir+lst[i]+'/48x48/categories/applications-accessories.png')
     else
     if FileExists(dir+lst[i]+'/24x24/places/user-home.png') then
     IList[k].SetImage(dir+lst[i]+'/24x24/places/user-home.png')
     except end;

     IList[k].SetPositions;
    Inc(k);
    Application.ProcessMessages;
    end;
    lst.Free;
  end;
end;

procedure TisFrm.PageControl1Change(Sender: TObject);
var i: Integer;
begin
case PageControl1.TabIndex of
0: for i:=0 to DLLength-1 do DList[i].SetPositions;
1: for i:=0 to ILLength-1 do IList[i].SetPositions;
2: for i:=0 to WLLength-1 do WList[i].SetPositions;
3: for i:=0 to SLLength-1 do SList[i].SetPositions;
end;
end;

procedure TisFrm.ScreensaversShow(Sender: TObject);
var lst: TStringList;i,k: Integer;
    cnf: TIniFile;
begin
if SLLength<1 then begin
 lst:=TStringList.Create;

 lst.Assign(FindAllFiles('/usr/share/applications/screensavers','*.desktop',false));

 for i:=0 to lst.Count-1 do begin
     k:=SLLength+1;
     SetLength(SList,k);
     SLLength:=k;
     Dec(k);
     SList[k]:=TListEntry.Create(isFrm);
     SList[k].Parent:=ScrollBox4;
     SList[k].UnButton.Tag:=k;
     SList[k].UnButton.OnClick:=@UninstallScreensaver;
     SList[k].srmId:=lst[i];

     cnf:=TIniFile.Create(lst[i]);
     SList[k].AppName:=cnf.ReadString('Desktop Entry','Name['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','');
     if SList[k].AppName='' then
     SList[k].AppName:=cnf.ReadString('Desktop Entry','Name','');

     SList[k].AppDesc:=cnf.ReadString('Desktop Entry','Comment['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','');
     if SList[k].AppDesc='' then
     SList[k].AppDesc:=cnf.ReadString('Desktop Entry','Comment','');

     SList[k].AppVersion:='';
     if SList[k].AppDesc='' then
     SList[k].AppDesc:='';
     SList[k].AppMn:='';
     SList[k].SetImage(GetDataFile('graphics/screensaver.png'));
     SList[k].SetPositions;
    Inc(k);
    Application.ProcessMessages;
    end;
    lst.Free;
  end;
end;

procedure TisFrm.WallpapersShow(Sender: TObject);
var lst: TStringList;i,k: Integer;SR: TSearchRec;
    cnf: TIniFile;
begin
if WLLength<1 then begin
 lst:=TStringList.Create;
if GetDistro.Desktop='KDE' then begin
//Load KDE4 Wallapers
if IsRoot then begin
if FindFirst('/usr/share/wallpapers/*', faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory = faDirectory then
        if (SR.Name<>' ') and (SR.Name <> '.') and (SR.name<>'..')then
          lst.Add(SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
end else
lst.Assign(FindAllFiles(GetEnvironmentVariable('HOME')+'/.kde/share/wallpapers/','*.*',true));

  for i:=0 to lst.Count-1 do begin
     k:=WLLength+1;
     SetLength(WList,k);
     WLLength:=k;
     Dec(k);
     WList[k]:=TListEntry.Create(isFrm);
     WList[k].Parent:=ScrollBox3;
     WList[k].UnButton.Tag:=k;
     WList[k].UnButton.OnClick:=@UninstallWallpaper;

     if IsRoot then begin
     //If application is in -su mode
     cnf:=TIniFile.Create('/usr/share/wallpapers/'+lst[i]+'/metadata.desktop');
     WList[k].srmId:='/usr/share/wallpapers/'+lst[i];
     WList[k].AppName:=cnf.ReadString('Desktop Entry','Name['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','');
     if WList[k].AppName='' then
     WList[k].AppName:=cnf.ReadString('Desktop Entry','Name',lst[i]);

     WList[k].AppMn:=cnf.ReadString('Desktop Entry','X-KDE-PluginInfo-Author','');
     cnf.Free;
     WList[k].AppVersion:='';
     WList[k].AppDesc:='';

     try
     WList[k].SetImage('/usr/share/wallpapers/'+lst[i]+'/contents/screenshot.png');
     except end;
     end else begin
     //If not
     WList[k].srmId:=lst[i];
     WList[k].AppName:=ExtractFileName(lst[i]);

     WList[k].AppDesc:='';
     WList[k].AppMn:='';

     try
     WList[k].SetImage(lst[i]);
     except end;
     end;

     WList[k].SetPositions;
    Inc(k);
    Application.ProcessMessages;
    end;
end else begin
//Load GNOME & Co. Wallpapers
 lst.Assign(FindAllFiles('/usr/share/backgrounds/','*.*',false));

 for i:=0 to lst.Count-1 do begin
     k:=WLLength+1;
     SetLength(WList,k);
     WLLength:=k;
     Dec(k);
     WList[k]:=TListEntry.Create(isFrm);
     WList[k].Parent:=ScrollBox3;
     WList[k].UnButton.Tag:=k;
     WList[k].srmId:=lst[i];

     WList[k].AppName:=ExtractFileName(lst[i]);

     WList[k].AppDesc:='';
     WList[k].AppMn:='';

     try
     WList[k].SetImage(lst[i]);
     except end;

     WList[k].SetPositions;
    Inc(k);
    Application.ProcessMessages;
    end;
  end;
   lst.Free;
  end;
end;

initialization
  {$I thinstall.lrs}

end.

