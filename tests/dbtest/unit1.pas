unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LiAppMgr, LiTypes, LiUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
var mgr: PLiAppManager;
begin
  mgr := li_mgr_new;
  li_mgr_update_appdb(@mgr);
  li_mgr_free(@mgr);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  perror('Test');
end;

end.

