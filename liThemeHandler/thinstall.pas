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
if Application.MessageBox(PChar(StringReplace(strRealUninstQ,'%a',DList[(Sender as TBitBtn).Tag].AppLabel.Caption,[rfReplaceAll])),PChar(strRmPkgQ),MB_YESNO) = IDYES then
if ExtractFileExt(DList[(Sender as TBitBtn).Tag].sid)='' then
DeleteDirectory(DList[(Sender as TBitBtn).Tag].sid,false)
else
DeleteFile(DList[(Sender as TBitBtn).Tag].sid);
end;

procedure TisFrm.UninstallWallpaper(Sender: TObject);
var t: TProcess;
begin
if Application.MessageBox(PChar(StringReplace(strRealUninstQ,'%a',DList[(Sender as TBitBtn).Tag].AppLabel.Caption,[rfReplaceAll])),PChar(strRmPkgQ),MB_YESNO) = IDYES then
if ExtractFileExt(DList[(Sender as TBitBtn).Tag].sid)='' then
DeleteDirectory(DList[(Sender as TBitBtn).Tag].sid,false)
else
if GetDistro.Desktop='GNOME' then begin
if not IsRoot then begin
       t:=TProcess.Create(nil);
         if GetDistro.DName='Ubuntu' then
            t.CommandLine := 'gksudo rm '+DList[(Sender as TBitBtn).Tag].sid
         else
          t.CommandLine := 'gnomesu rm '+DList[(Sender as TBitBtn).Tag].sid;
        t.Options:=[];
        t.Execute;
        t.Free;
   end;
 end else
DeleteFile(DList[(Sender as TBitBtn).Tag].sid);
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
     DList[k].sid:=dir+lst[i];

     cnf:=TIniFile.Create(dir+lst[i]+'/index.theme');
     DList[k].AppLabel.Caption:=cnf.ReadString('Desktop Entry','Name',lst[i]);
     DList[k].DescLabel.Caption:=cnf.ReadString('Desktop Entry','Comment['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','');
     if DList[k].DescLabel.Caption='' then
     DList[k].DescLabel.Caption:=cnf.ReadString('Desktop Entry','Comment','');

     DList[k].VLabel.Visible:=false;
     if DList[k].DescLabel.Caption='' then
     DList[k].DescLabel.Visible:=false;
     DList[k].mnLabel.Visible:=false;

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
  headImage.Picture.LoadFromFile(ExtractFilePath(Application.ExeName)+'graphics/ligraphic-header.png');
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
     IList[k].sid:=lst[i];

     IList[k].AppLabel.Caption:=lst[i];

     IList[k].VLabel.Visible:=false;
     IList[k].DescLabel.Visible:=false;
     IList[k].mnLabel.Visible:=false;

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
     SList[k].sid:=lst[i];

     cnf:=TIniFile.Create(lst[i]);
     SList[k].AppLabel.Caption:=cnf.ReadString('Desktop Entry','Name['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','');
     if SList[k].AppLabel.Caption='' then
     SList[k].AppLabel.Caption:=cnf.ReadString('Desktop Entry','Name','');

     SList[k].DescLabel.Caption:=cnf.ReadString('Desktop Entry','Comment['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','');
     if SList[k].DescLabel.Caption='' then
     SList[k].DescLabel.Caption:=cnf.ReadString('Desktop Entry','Comment','');

     SList[k].VLabel.Visible:=false;
     if SList[k].DescLabel.Caption='' then
     SList[k].DescLabel.Visible:=false;
     SList[k].mnLabel.Visible:=false;
     SList[k].SetImage(ExtractFilePath(Application.Exename)+'graphics/screensaver.png');
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
     WList[k].sid:='/usr/share/wallpapers/'+lst[i];
     WList[k].AppLabel.Caption:=cnf.ReadString('Desktop Entry','Name['+Copy(GetEnvironmentVariable('LANG'), 1, 2)+']','');
     if WList[k].AppLabel.Caption='' then
     WList[k].AppLabel.Caption:=cnf.ReadString('Desktop Entry','Name',lst[i]);

     WList[k].mnLabel.Caption:=cnf.ReadString('Desktop Entry','X-KDE-PluginInfo-Author','');
     cnf.Free;
     WList[k].VLabel.Visible:=false;
     WList[k].DescLabel.Visible:=false;
     if WList[k].mnLabel.Caption='' then
     WList[k].mnLabel.Visible:=false;

     try
     WList[k].SetImage('/usr/share/wallpapers/'+lst[i]+'/contents/screenshot.png');
     except end;
     end else begin
     //If not
     WList[k].sid:=lst[i];
     WList[k].AppLabel.Caption:=ExtractFileName(lst[i]);

     WList[k].VLabel.Visible:=false;
     WList[k].DescLabel.Visible:=false;
     WList[k].mnLabel.Visible:=false;

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
     WList[k].sid:=lst[i];

     WList[k].AppLabel.Caption:=ExtractFileName(lst[i]);

     WList[k].VLabel.Visible:=false;
     WList[k].DescLabel.Visible:=false;
     WList[k].mnLabel.Visible:=false;

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

