unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, MD5;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TLabeledEdit;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
if OpenDialog1.Execute then
 Edit1.Text:=MD5.MDPrint(MD5.MD5File(OpenDialog1.FileName,1034));
end;

initialization
  {$I unit1.lrs}

end.

