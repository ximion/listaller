//Created by Matthias Klumpp, licensed under GPLv3
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ipkpackage, gpgsign;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure testProgress(pos: Integer; user_data: Pointer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.testProgress(pos: Integer; user_data: Pointer);
begin
 ProgressBar1.Position:=pos;
 Application.ProcessMessages;
end;

procedure TForm1.Button1Click(Sender: TObject);
var test: TLiPackager;
begin
 test:=TLiPackager.Create('/home/matthias/Desktop/test.ipk');
 test.BaseDir:='/tmp/';
 if not test.AddFile('/home/matthias/Desktop/littemp.txt') then
  ShowMessage('Error!');
 test.AddFile('/home/matthias/Desktop/a.png');
 test.AddFile('/home/matthias/Desktop/b.jpg');
 test.Finalize;
 test.SignPackage;
 test.ProduceIPKPackage;
 test.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var test: TLiUnpacker;
begin
  if OpenDialog1.Execute then
  begin
    test:=TliUnpacker.Create(OpenDialog1.FileName);
    test.OnProgress:=@testProgress;
    test.Decompress;
    if test.CheckSignature then
     ShowMessage('OKAY')
    else
     ShowMessage('Wrong!');
    test.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var test: TGPGSignWrapper;
begin
if OpenDialog1.Execute then
  begin
   test:=TGPGSignWrapper.Create;
   test.FileName:=OpenDialog1.FileName;
   if not test.Verify(OpenDialog1.FileName+'.asc') then
    ShowMessage('Verification failed!');
   test.Free;
  end;
end;

initialization
  {$I unit1.lrs}

end.

