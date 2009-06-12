{ settings.pas
  Copyright (C) Listaller Project 2008-2009

  settings.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  settings.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Change Listaller's settings
unit settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  CheckLst, StdCtrls, Buttons, ExtCtrls, LCLType, manager, IniFiles, Spin,
  Process, LiCommon, trstrings, gettext, Menus, ipkhandle;

type

  { TFmConfig }

  TFmConfig = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CbShowPkMon: TCheckBox;
    CheckBox4: TCheckBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    edtUsername: TLabeledEdit;
    edtPasswd: TLabeledEdit;
    Label5: TLabel;
    edtFTPProxy: TLabeledEdit;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    UListBox1: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    PageControl1: TPageControl;
    MainPage: TTabSheet;
    Panel1: TPanel;
    UpdPage: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CbShowPkMonChange(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MainPageShow(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure UListBox1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FmConfig: TFmConfig;

implementation

{ TFmConfig }

procedure TFmConfig.FormCreate(Sender: TObject);
var i: Integer;tmp: TStringList;
begin
  PageControl1.ActivePageIndex:=0;
  
  tmp:=TStringList.Create;
  if not FileExists(RegDir+'updates.list') then begin
   tmp.Add('Listaller UpdateSources-pk0.8');
   tmp.SaveToFile(RegDir+'updates.list');
  end;
  tmp.LoadFromFile(RegDir+'updates.list');
  for i:=1 to tmp.Count-1 do begin
  UListBox1.items.Add(copy(tmp[i],pos(' <',tmp[i])+2,length(tmp[i])-pos(' <',tmp[i])-2)+' ('+copy(tmp[i],3,pos(' <',tmp[i])-3)+')');
  UListBox1.Checked[UListBox1.Items.Count-1]:=tmp[i][1]='-';
  end;

  LoadStockPixmap(STOCK_CLOSE,ICON_SIZE_BUTTON,BitBtn3.Glyph);

  //Translate...
  UpdPage.Caption:=strUpdSources;
  BitBtn3.Caption:=strClose;
  BitBtn2.Caption:=strDelSrc;
  BitBtn1.Caption:=strCheckForUpd;
  Label1.Caption:=strListofSrc;
  edtUsername.Caption:=strUsername+':';
  edtPasswd.Caption:=strPassword+':';
  CheckBox4.Caption:=strEnableProxy;
  GroupBox1.Caption:=strProxySettings;
  CheckBox2.Caption:=strAutoLoadDep;
  CbShowPkMon.Caption:=strShowPkMon;
end;

procedure TFmConfig.FormShow(Sender: TObject);
begin
 BitBtn2.Left:=6;
 BitBtn3.Left:=640-BitBtn3.Width;
end;

procedure TFmConfig.MainPageShow(Sender: TObject);
var cnf:TIniFile;
begin
  cnf:=TIniFile.Create(ConfigDir+'config.cnf');
  CheckBox4.Checked:=cnf.ReadBool('Proxy','UseProxy',false);
  Edit1.Text:=cnf.ReadString('Proxy','Server','');
  SpinEdit1.Value:=cnf.ReadInteger('Proxy','Port',0);
  CbShowPkMon.Checked:=cnf.ReadBool('MainConf','ShowPkMon',false);
  cnf.free;
if Edit1.Text='' then begin
if (mnFrm.DInfo.DBase='GNOME')and(FileExists('/usr/bin/gconftool-2')) then
begin
if CmdResult('gconftool-2 -g /system/http_proxy/use_http_proxy')='true' then CheckBox4.Checked:=true
else CheckBox4.Checked:=false;
Edit1.Text:=CmdResult('gconftool-2 -g /system/http_proxy/host');
SpinEdit1.Value:=StrToInt(CmdResult('gconftool-2 -g /system/http_proxy/port'));
end;
end;
end;

procedure TFmConfig.PageControl1Change(Sender: TObject);
begin

end;

procedure TFmConfig.UListBox1Click(Sender: TObject);
var uconf: TStringList;h: String;
begin
uconf:=TStringList.Create;
uconf.LoadFromFile(RegDir+'updates.list');
h:=uconf[UListBox1.ItemIndex+1];
if UListBox1.Checked[UListBox1.ItemIndex] then
h[1]:='-' else h[1]:='#';
uconf[UListBox1.ItemIndex+1]:=h;
uconf.SaveToFile(RegDir+'updates.list');
uconf.Free;
end;


  
procedure TFmConfig.BitBtn3Click(Sender: TObject);
var p: String;cnf: TIniFile;
begin
  p:=ConfigDir;
  cnf:=TIniFile.Create(p+'config.cnf');
  cnf.WriteString('Proxy','hServer',Edit1.Text);
  cnf.WriteInteger('Proxy','hPort',SpinEdit1.Value);
  //
  cnf.WriteString('Proxy','Username',edtUsername.Caption);
  cnf.WriteString('Proxy','Password',edtPasswd.Caption);
  
  cnf.WriteString('Proxy','fServer',edtFTPProxy.Caption);
  cnf.WriteInteger('Proxy','fPort',SpinEdit2.Value);
  cnf.Free;
  Close;
end;

procedure TFmConfig.CheckBox1Change(Sender: TObject);
var h: String;ini: TIniFile;
begin
  h:=ConfigDir;
  ini:=TIniFile.Create(h+'config.cnf');
  ini.WriteBool('MainConf','DistroWarning',(Sender as TCheckBox).Checked);
  ini.Free;
end;

procedure TFmConfig.CheckBox2Change(Sender: TObject);
var h: String;ini: TIniFile;
begin
  h:=ConfigDir;
  ini:=TIniFile.Create(h+'config.cnf');
  ini.WriteBool('MainConf','AutoDepLoad',(Sender as TCheckBox).Checked);
  ini.Free;
end;

procedure TFmConfig.CbShowPkMonChange(Sender: TObject);
var h: String;ini: TIniFile;
begin
  h:=ConfigDir;
  ini:=TIniFile.Create(h+'config.cnf');
  ini.WriteBool('MainConf','ShowPkMon',(Sender as TCheckBox).Checked);
  ini.Free;
end;

procedure TFmConfig.CheckBox4Change(Sender: TObject);
var p: String;cnf: TIniFile;
begin
  if (Sender as TCheckBox).Checked then begin
  Label3.Enabled:=true;
  Edit1.Enabled:=true;
  SpinEdit1.Enabled:=true;
  edtUsername.Enabled:=true;
  edtPasswd.Enabled:=true;
  edtFTPProxy.Enabled:=true;
  spinEdit2.Enabled:=true;
  Label4.Enabled:=true;
  Label5.Enabled:=true;
  end else begin
  Label3.Enabled:=false;
  Edit1.Enabled:=false;
  SpinEdit1.Enabled:=false;
  Label4.Enabled:=false;
  edtUsername.Enabled:=false;
  edtPasswd.Enabled:=false;
  edtFTPProxy.Enabled:=false;
  spinEdit2.Enabled:=false;
  Label5.Enabled:=false;
  end;
  p:=ConfigDir;
  cnf:=TIniFile.Create(p+'config.cnf');
  cnf.WriteBool('Proxy','UseProxy',(Sender as TCheckBox).Checked);
  cnf.Free;
end;

procedure TFmConfig.BitBtn2Click(Sender: TObject);
var uconf: TStringList;
begin
if Application.MessageBox(PChar(strRmSrcQ),PChar(strRmSrcQC),MB_YESNO)=IDYES then begin
uconf:=tStringList.Create;
uconf.LoadFromFile(RegDir+'updates.list');
uconf.Delete(UListBox1.ItemIndex+1);
uconf.SaveToFile(RegDir+'updates.list');
uconf.Free;
UListBox1.Items.Delete(UListBox1.ItemIndex);
ShowMessage('Source deleted!');
end;
end;

procedure TFmConfig.BitBtn1Click(Sender: TObject);
begin
  mnFrm.Process1.CommandLine:=ExtractFilePath(Application.ExeName)+'liupdate -show';
  mnFrm.Process1.Execute;
end;

initialization
  {$I settings.lrs}

end.

