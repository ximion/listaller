//Created by Matthias Klumpp, licensed under GPlv3
//Unit to test the PackageKit bindings implementation
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, packagekit, Messages, pkdesktop, glib2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure pkitProgress(pos: Integer;user_data: Pointer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;
  pkit: TPackageKit;

implementation

{ TForm1 }

procedure ArrFunc(data: GPointer;user_data: GPointer);
begin
 ShowMessage(PGChar(data));
end;

procedure TForm1.Button1Click(Sender: TObject);
var p,q: Pointer;tmp: TStringList;
begin
  pkit:=TPackageKit.Create;
  pkit.OnProgress:=@pkitProgress;
  tmp:=TStringList.Create;
  pkit.RsList:=tmp;
  pkit.PkgNameFromFile('/usr/share/applications/nemiver.desktop');
  //pkit.RemovePkg('amor');

  ShowMessage(tmp[0]);
  //ShowMessage(h[0]);
  ShowMessage('OK, Done'#10+pkit.LastErrorMessage);
  pkit.Free;

{ p:=pk_desktop_new;
 pk_desktop_open_database(p,nil);
 q:=pk_desktop_get_files_for_package(p,'lazarus',nil);

 g_ptr_array_foreach(q,@ArrFunc,nil);  }

end;

procedure TForm1.pkitProgress(pos: Integer;user_data: Pointer);
begin
  ProgressBar1.Position:=pos;
  Application.ProcessMessages;
end;

initialization
  {$I unit1.lrs}

end.

