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
   Caption:=strAboutListaller;
   Image1.Picture.LoadFromFile('/usr/share/pixmaps/listaller.png');
   Label2.Caption:=strVersion+': '+LiVersion;
   Label5.Caption:=strUseLaunchpadForBugs;
   Label3.Caption:=StringReplace(strLinDesk,'%de',GetDistro.Desktop,[rfReplaceAll]);
   AboutSheet.Caption:=strAbout;
   AuthorSheet.Caption:=strAuthors;
   BitBtn1.Caption:=strClose;
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

