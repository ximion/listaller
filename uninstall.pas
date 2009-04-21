{ uninstall.pas
  Copyright (C) Listaller Project 2008-2009

  uninstall.pas is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  uninstall.pas is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** Window that shows the progress while uninstalling applications
unit uninstall;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, IniFiles, LCLType, common, Buttons, ExtCtrls, process,
  trstrings, FileUtil, distri, ipkhandle;

type

  { TRMForm }
  TRMForm = class(TForm)
    BitBtn1: TBitBtn;
    GetOutPutTimer: TIdleTimer;
    Label1: TLabel;
    Memo1: TMemo;
    Process1: TProcess;
    UProgress: TProgressBar;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GetOutPutTimerTimer(Sender: TObject);
  private
    { private declarations }
    FActiv: Boolean;
    //** Method that removes MOJO/LOKI installed applications @param dsk Path to the .desktop file of the application
    procedure UninstallMojo(dsk: String);
  public
    { public declarations }
  end; 

var
 //** Window that shows the progress of the uninstallation
  RMForm: TRMForm;

implementation

uses manager;

{ TRMForm }

procedure TRMForm.FormShow(Sender: TObject);
begin
Label1.Caption:=strWaiting;
Caption:=StringReplace(strRMAppC,'%a','...',[rfReplaceAll])
end;

procedure TRMForm.GetOutPutTimerTimer(Sender: TObject);
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

    DoStuffForProcess(Process1, Memo1);
  until noMoreOutput;
if Process1.ExitStatus>0 then begin
    GetOutputTimer.Enabled:=false;
    ShowMessage(strRMError);
    Memo1.Lines.SaveTofile(ConfigDir+'uninstall.log');
    halt;
    exit;
  end;
end;

procedure LogAdd(s: String);
begin
writeLn(s);
RMForm.Memo1.Lines.add(s);
end;

//Uninstall Mojo and LOKI Setups
procedure TRMForm.UninstallMojo(dsk: String);
var inf: TIniFile;tmp: TStringList;t: TProcess;mandir: String;
begin
LogAdd('Package could be installed with MoJo/LOKI...');
inf:=TIniFile.Create(dsk);
if not DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))) then begin
writeLn('Listaller cannot handle this installation!');
ShowMessage(strCannotHandleRM);inf.Free;end else
if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))+'.mojosetup') then begin
//MOJO
mandir:=ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))+'.mojosetup';
inf.Free;
LogAdd('Mojo manifest found.');
UProgress.Position:=40;
tmp:=TStringList.Create;
tmp.Assign(FindAllFiles(mandir+'/manifest','*.xml',false));
if tmp.Count<=0 then exit;
UProgress.Position:=50;
LogAdd('Uninstalling application...');
 t:=TProcess.Create(nil);
 t.CommandLine:=mandir+'/mojosetup uninstall '+copy(ExtractFileName(tmp[0]),1,pos('.',ExtractFileName(tmp[0]))-1);
 t.Options:=[poUsePipes,poWaitonexit];
 tmp.Free;
 UProgress.Position:=60;
 t.Execute;
 t.Free;
 UProgress.Position:=100;
 mnFrm.LoadEntries;
end else
//LOKI
if DirectoryExists(ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))+'.manifest') then begin
 UProgress.Position:=50;
 LogAdd('LOKI setup detected.');
 LogAdd('Uninstalling application...');
 Application.ProcessMessages;
 t:=TProcess.Create(nil);
 t.CommandLine:=ExtractFilePath(inf.ReadString('Desktop Entry','Exec','?'))+'/uninstall';
 t.Options:=[poUsePipes,poWaitonexit];

 UProgress.Position:=60;
 t.Execute;
 t.Free;
 UProgress.Position:=100;
 mnFrm.LoadEntries;
end else begin
 writeLn('Listaller cannot handle this installation type!');
 ShowMessage(strCannotHandleRM);inf.Free;end;
end;

procedure TRMForm.FormActivate(Sender: TObject);
var f,g: String; t:TProcess;cf: TIniFile;tmp: TStringList;
begin
if FActiv then begin
FActiv:=false;
Memo1.Lines.Clear;
BitBtn1.Enabled:=false;
with mnFrm do begin
if (uID<0) then begin
ShowMessage('Wrong selection!');
uID:=-1;
close;
exit;
end;
Memo1.Lines.Add('Connecting to PackageKit... (run "pkmon" to see the actions)');
GetOutPutTimer.Enabled:=false;
UProgress.Position:=0;
RMForm.Caption:=StringReplace(strRMAppC,'%a',AList[uID].AppName,[rfReplaceAll]);
if Application.MessageBox(PAnsiChar(StringReplace(strRealUninstQ,'%a',AList[uID].AppName,[rfReplaceAll])),'Uninstall?',MB_YESNO)=IDYES then begin
LogAdd('Reading application information...');
RMForm.Label1.Caption:='Reading application information...';
GetOutPutTimer.Enabled:=true;

if IdList[uID][1]='~' then begin
if FileExists(RegDir+AList[uID].AppName+'~'+copy(IdList[uID],2,length(IdList[uID]))+'/appfiles.list') then
begin
tmp:=TStringList.Create;
tmp.LoadFromFile(RegDir+AList[uID].AppName+'~'+copy(IdList[uID],2,length(IdList[uID]))+'/appfiles.list');
UProgress.Max:=((tmp.Count)*10)+4;
tmp.Free;
end;
UProgress.Position:=UninstallIPKApp(AList[uID].AppName,copy(IdList[uID],2,length(IdList[uID])),Memo1.Lines);
LogAdd('Finished!');
Memo1.Lines.SaveToFile(ConfigDir+'uninstall.log');
ShowMessage(strUnistSuccess);
//Controls wieder aktivieren
BitBtn1.Enabled:=true;
SWBox.Enabled:=true;
LoadEntries;
RMForm.Close;
exit;
 end else begin //Autopackage
 if IdList[uID][1]='!' then begin
 t:=TProcess.Create(nil);
 t.CommandLine:=copy(IdList[uID],2,length(IdList[uID]));
 t.Options:=[poUsePipes,poWaitonexit];
 t.Execute;
 t.Free;
 LoadEntries;
 RMForm.Close;
 exit;
 end;
end;

//Uninstall external application(s) (only possible if user is root)
if not IsRoot then begin UninstallMojo(IdList[uID]);exit;end;

if (IdList[uID][1]='/')
then begin
// /!\
///////////////////////////////////////////////////////

ShowPKMon();

Application.ProcessMessages;
writeLn('Detecting package...');
f:=CmdResult(pkit+'--s-file '+IdList[uID]);
if f='Failed!' then begin UninstallMojo(IdList[uID]);exit;end;
if f='PackageKit problem.' then begin ShowMessage(strPKitProbPkMon);exit;end;
g:='';

Application.ProcessMessages;
writeLn('Looking for reverse-dependencies...');
g:=CmdResult(pkit+'--get-requires '+f);
g:=copy(g,pos(f,g)+length(f),length(g));

LogAdd('Package detected: '+f);
if (StringReplace(g,' ','',[rfReplaceAll])='')or
(Application.MessageBox(PAnsiChar(
StringReplace(StringReplace(StringReplace(strRMPkg,'%p',f,[rfReplaceAll]),'%a',AList[uID].AppName,[rfReplaceAll]),'%pl',PAnsiChar(g),[rfReplaceAll])
),PChar(strRMPkgQ),MB_YESNO)=IDYES)
then begin

LogAdd('Uninstalling '+f+' ...');
UProgress.Position:=60;
GetOutPutTimer.Enabled:=true;
Process1.CommandLine:=pkit+'--remove '+f;
Process1.Execute;
UProgress.Position:=78;
while Process1.Running do Application.ProcessMessages;
if Process1.ExitStatus>0 then begin
ShowMessage(strRmError);exit;RMForm.Close;end;

UProgress.Position:=100;
LogAdd('Done.');
RMForm.Close;
LoadEntries;
exit;
end else RMForm.close;

////////////////////////////////////////////////////////////////////////////
//
 end;
  end;
end;
  mnFrm.uID:=-1;
BitBtn1.Enabled:=true;
GetOutPutTimer.Enabled:=false;
end else Close;
end;

procedure TRMForm.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TRMForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FActiv:=true;
end;

procedure TRMForm.FormCreate(Sender: TObject);
begin
  FActiv:=true;
end;

initialization
  {$I uninstall.lrs}

end.

