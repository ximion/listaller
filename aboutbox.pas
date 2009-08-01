unit aboutbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, liCommon, trstrings, distri;

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
    Memo1: TMemo;
    PageControl1: TPageControl;
    AboutSheet: TTabSheet;
    AuthorSheet: TTabSheet;
    Panel1: TPanel;
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
   Caption:=rsAboutListaller;
   Image1.Picture.LoadFromFile('/usr/share/pixmaps/listaller.png');
   Label2.Caption:=rsVersion+': '+LiVersion;
   Label5.Caption:=rsUseLaunchpadForBugs;
   Label3.Caption:=Format(rsLinDesk,[GetDistro.Desktop]);
   AboutSheet.Caption:=rsAbout;
   AuthorSheet.Caption:=rsAuthors;
   BitBtn1.Caption:=rsClose;
   {$IFDEF LCLgtk2}
    Label1.Caption:='Listaller (GTK+)';
   {$ENDIF}
   {$IFDEF LCLQt}
    Label1.Caption:='Listaller (Qt)';
   {$ENDIF}
end;

procedure TFmAbout.BitBtn1Click(Sender: TObject);
begin
  close;
end;

initialization
  {$I aboutbox.lrs}

end.

