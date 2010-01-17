{ Copyright (C) 2009 Matthias Klumpp

  Authors:
   Matthias Klumpp

  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation; version 3.
  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.
  You should have received a copy of the GNU General Public License v3
  along with this program.  If not, see <http://www.gnu.org/licenses/>.}
//** The "About Listaller" information window
unit aboutbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, liBasic, trstrings, distri;

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
    Memo2: TMemo;
    PageControl1: TPageControl;
    AboutSheet: TTabSheet;
    AuthorSheet: TTabSheet;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
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
   Memo2.Text:=rsTranslators;
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

