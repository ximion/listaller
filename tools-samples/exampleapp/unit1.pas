unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;
  ID: Integer=1;

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Hello world!');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
with Label1 do begin
case ID of
0: Caption:='Hello!!';
1: Caption:='This application was installed with Listaller...';
2: Caption:='...the simple installer for all Linux distributions';
3: Caption:='This is a Demo-application';
4: Caption:='Enjoy!';
end;
end;
Inc(ID);
if ID>4 then ID:=0;
end;

initialization
  {$I unit1.lrs}

end.

