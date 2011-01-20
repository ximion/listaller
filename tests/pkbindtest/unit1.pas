(* Created by Matthias Klumpp <matthias@nlinux.org>
 *
 * Licensed under GPLv3+
 *)
//Unit to test the PackageKit bindings implementation
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, glib2, Classes, Dialogs,
  ComCtrls, Controls, FileUtil, Graphics, Messages, StdCtrls, SysUtils,
  Installer, LResources, PackageKit;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure pkitProgress(pos: Integer; user_data: Pointer);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure ArrFunc(Data: GPointer; user_data: GPointer);
begin
  ShowMessage(PGChar(Data));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  p, q: Pointer;
  pkit: TPackageKit;
  pkit2: TPackageKit;
begin
  //Run async PackageKit resolving
  pkit := TPackageKit.Create;
  pkit.OnProgress := @pkitProgress;
  pkit.AsyncMode := true;

  pkit2 := TPackageKit.Create;
  pkit.AsyncMode := true;
  pkit.PkgNameFromFile('/usr/share/applications/nemiver.desktop');
  pkit2.PkgNameFromFile('/usr/share/applications/vlc.desktop');


  if pkit.RList.Count > 0 then
    ShowMessage(pkit.RList[0].PackageId)
  else
    ShowMessage('Pkit1 failed.');
  if pkit2.RList.Count > 0 then
    ShowMessage(pkit2.RList[0].PackageId)
  else
    ShowMessage('Pkit2 failed.');

  ShowMessage('OK, Done'#10+pkit.LastErrorMessage);
  pkit.Free;
  pkit2.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  pkit: TPackageKit;
begin
  pkit := TPackageKit.Create;
  pkit.OnProgress := @pkitProgress;

  pkit.PkgNameFromFile('/usr/share/applications/nemiver.desktop');
  if pkit.RList.Count > 0 then
    ShowMessage('Successful_1'#10+pkit.RList[0].PackageId+#10+'Code: '+
      IntToStr(Integer(pkit.PkExitStatus))+#10'Message:'#10+pkit.LastErrorMessage)
  else
    ShowMessage('Failed_1'#10'Code: '+IntToStr(Integer(pkit.PkExitStatus))+#10'Message:'#10+pkit.LastErrorMessage);
  pkit.PkgNameFromFile('/usr/share/applications/vlc.desktop');
  if pkit.RList.Count > 0 then
    ShowMessage('Successful_1'#10+pkit.RList[0].PackageId+#10+'Code: '+
      IntToStr(Integer(pkit.PkExitStatus)))
  else
    ShowMessage('Failed_1'#10'Code: '+IntToStr(Integer(pkit.PkExitStatus))+#10'Message:'#10+pkit.LastErrorMessage);
  pkit.Free;
end;


procedure TForm1.Button3Click(Sender: TObject);
var
  pkit: TPackageKit;
begin
  pkit := TPackageKit.Create;
  pkit.OnProgress := @pkitProgress;
  pkit.RemovePkg('amor');
  ShowMessage('Executed.');
  pkit.Free;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  pkit: TPackageKit;
begin
  pkit := TPackageKit.Create;
  pkit.OnProgress := @pkitProgress;
  pkit.InstallPkg('amor');
  ShowMessage('Executed.');
  pkit.Free;
end;

procedure TForm1.pkitProgress(pos: Integer; user_data: Pointer);
begin
  ProgressBar1.Position := pos;
  Application.ProcessMessages;
end;

end.

