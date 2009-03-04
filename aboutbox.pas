unit aboutbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, utilities, trstrings, distri;

type

  { TFmAbout }

  TFmAbout = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PageControl1: TPageControl;
    AboutSheet: TTabSheet;
    AuthorSheet: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FmAbout: TFmAbout;

implementation

{ TFmAbout }

procedure TFmAbout.FormShow(Sender: TObject);
begin
   Image1.Picture.LoadFromFile('/usr/share/pixmaps/listaller.png');
   Label2.Caption:='Version '+LiVersion;
   Label3.Caption:='Running under '+GetDistro.Desktop+' (or similar)';
   AboutSheet.Caption:=strAbout;
   AuthorSheet.Caption:=strAuthors;
   BitBtn1.Caption:=strClose;
end;

procedure TFmAbout.BitBtn1Click(Sender: TObject);
begin
  close;
end;

initialization
  {$I aboutbox.lrs}

end.

