{ updexec.pas
  Copyright (C) Listaller Project 2008

  updexec.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  updexec.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Executes software updates (GUI based)
unit updexec;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, HTTPSend, FileUtil, AbUnZper, AbArcTyp, Process, common, IniFiles,
  blcksock, trStrings, ipkhandle;

type

  { TUExecFm }

  TUExecFm = class(TForm)
    Button1: TButton;
    ILabel: TLabel;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    ProgressBar2: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    procedure HookSock(Sender: TObject; Reason:THookSocketReason; const Value: string);
  public
    { public declarations }
  end; 

var
  UExecFm: TUExecFm;

implementation

uses mnupdate;

{ TUExecFm }

procedure TUExecFm.FormShow(Sender: TObject);
begin
end;

procedure TUExecFm.Button1Click(Sender: TObject);
begin
  Close;
  Form1.BitBtn2Click(nil);
end;

var fstact: Boolean=true;
    HTTP: THTTPSend; //<- Has to be global
    
procedure TUExecFm.HookSock(Sender: TObject; Reason: THookSocketReason;
const Value: string);
begin
if HTTP.DownloadSize>100 then begin
ProgressBar2.Max:=HTTP.DownloadSize;
ProgressBar2.Position:=HTTP.Document.Size;
end;
Application.ProcessMessages;
end;

procedure WriteLog(s: String);
begin
writeLn(s);
UExecFm.Memo1.Lines.Add(s);
end;

procedure TUExecFm.FormActivate(Sender: TObject);
var
  i,j,k: Integer;z: TAbUnZipper;
  c: TProcess;
  reg,cnf,dsk: TIniFile;
  s: TStringList;
begin
if fstact then begin
fstact:=false;
Memo1.Lines.Clear;
Memo1.Lines.Add('Log:');
    CreateDir('/tmp/liupd/');
    c:=tprocess.create(nil);
    c.Options:=[poUsePipes,poWaitonexit];
    reg:=TIniFile.Create(RegDir+'appreg.lst');
    with Form1 do begin
  for j:=0 to length(ulist)-1 do begin
  if CheckListBox1.Checked[j] then begin
    ProgressBar1.Max:=(ulist[j].Count div 2);
    ProgressBar2.Position:=100;

    WriteLog('Begin.');
    for i:=0 to ulist[j].Count-1 do
    if i mod 2 = 0 then begin
    WriteLog('GET: '+ulist[j][i]);
    ILabel.Caption:='Download...';
    Application.ProcessMessages;

    HTTP := THTTPSend.Create;
    HTTP.Sock.OnStatus:=HookSock;
    HTTP.UserAgent:='Listaller-Update';
    cnf:=TInifile.Create(ConfigDir+'config.cnf');
    if cnf.ReadBool('Proxy','UseProxy',false) then begin
    HTTP.ProxyPort:=cnf.ReadString('Proxy','Port','');
    HTTP.ProxyHost:=cnf.ReadString('Proxy','Server','');
    end;
    cnf.Free;
    try
    HTTP.HTTPMethod('GET', ulist[j][i]);

    if ExtractFileExt(ulist[j][i])<>'' then
    HTTP.Document.SaveToFile('/tmp/liupd/'+StringReplace(ExtractFileName(ulist[j][i]),ExtractFileExt(ulist[j][i]),'.zip',[rfReplaceAll]))
    else
    HTTP.Document.SaveToFile('/tmp/liupd/'+ExtractFileName(ulist[j][i])+'.zip');
    try
    z:=TAbUnZipper.Create(nil);
    WriteLog('Unpacking file...');
    ILabel.Caption:='Install...';
    Application.ProcessMessages;
    if ExtractFileExt(ulist[j][i])<>'' then
    z.FileName:='/tmp/liupd/'+StringReplace(ExtractFileName(ulist[j][i]),ExtractFileExt(ulist[j][i]),'.zip',[rfReplaceAll])
    else
    z.FileName:='/tmp/liupd/'+ExtractFileName(ulist[j][i])+'.zip';
    
    z.ExtractOptions:=[eoCreateDirs]+[eoRestorePath];
    z.BaseDirectory:=DeleteModifiers(ulist[j][i+1]);
    Application.ProcessMessages;

    DeleteFile(DeleteModifiers(ulist[j][i+1])+'/'+ExtractFileName(ulist[j][i])); //Delete old File (not always necessary, but sometimes needed)

    z.ExtractFiles(ExtractFileName(ulist[j][i]));
    Application.ProcessMessages;

    if(pos('.desktop',LowerCase(ExtractFileName(ulist[j][i])))>0) then
       begin
       WriteLog('Writing configuration for '+ExtractFileName(ulist[j][i]));
       dsk:=TIniFile.Create(ulist[j][i]);
       if dsk.ValueExists('Desktop Entry','Icon') then
       dsk.WriteString('Desktop Entry','Icon',SyblToPath(dsk.ReadString('Desktop Entry','Icon','*')));
       if dsk.ValueExists('Desktop Entry','Exec') then
       dsk.WriteString('Desktop Entry','Exec',SyblToPath(dsk.ReadString('Desktop Entry','Exec','*')));
       dsk.Free;
       end;

    if(pos(' <setvars>',LowerCase(ExtractFileName(ulist[j][i])))>0) then
       begin
       WriteLog('Writing configuration for '+ExtractFileName(ulist[j][i]));
       s:=TStringList.Create;
       s.LoadFromFile(ulist[j][i]);
       for k:=0 to s.Count-1 do
       s[k]:=SyblToPath(s[k]);
       s.SaveToFile(ulist[j][i]);
       s.Free;
       end;

    except
   ShowMessage(strExtractError);
   WriteLog(strUpdConfError);
    z.Free;
    exit;
    end;

WriteLog('chmod...');
ILabel.Caption:='Assign rights..';
if pos(' <chmod:',ulist[j][i+1])>0 then begin
c.CommandLine := 'chmod '+copy(ulist[j][i+1],pos(' <chmod:',ulist[j][i+1])+8,3)+SyblToPath(ulist[j][i+1])+'/'+ExtractFileName(DeleteModifiers(ulist[j][i+1]));
c.Execute;
end else begin
c.CommandLine :='chmod 755 '+DeleteModifiers(SyblToPath(ulist[j][i+1]))+'/'+ExtractFileName(ulist[j][i]);
c.Execute;
end;
Memo1.Lines.Add('Finishing...');
    ProgressBar1.Position:=ProgressBar1.Position+1;
    z.Free;
    Memo1.Lines.Add('Okay');
      finally
    HTTP.Free;
  end;
    end;
    if anotes[j].NVersion<>'' then
   reg.WriteString(CheckListBox1.Items[j]+anotes[j].ID,'Version',anotes[j].NVersion);
  end; //End of Check-Test
 end; //End Loop j

for j:=0 to length(ulist)-1 do ulist[j].Free;
end;

  WriteLog('Cleaning up...');
  c.Free;
  reg.Free;
  FileUtil.DeleteDirectory('/tmp/liupd/',false);
  Button1.Enabled:=true;
  WriteLog('Update finished!');
  ILabel.Caption:=strSuccess;
end;
end;

procedure TUExecFm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  fstact:=true;
end;

procedure TUExecFm.FormCreate(Sender: TObject);
begin
  Memo1.Font.Color:=clWhite;
  Caption:=strUpdInstalling;
  Button1.Caption:=strClose;
end;

initialization
  {$I updexec.lrs}

end.

