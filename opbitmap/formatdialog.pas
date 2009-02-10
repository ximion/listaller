unit formatdialog;

{$MODE objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons, opbitmap,
  {$ifndef OpbCompat}GraphicEx,{$endif} opbitmapformats,
  Spin, StdCtrls, ComCtrls, ExtCtrls;

type

  { TFormatDialog }

  TFormatDialog = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PageControl1: TPageControl;
    SpinEdit1: TSpinEdit;
    TabJPEG: TTabSheet;
    TabBMP: TTabSheet;
    TabGIF: TTabSheet;
    TabPNG: TTabSheet;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    fImage: TCanvasOPBitmap;
    { private declarations }
  public
    { public declarations }
    property Image: TCanvasOPBitmap read fImage write fImage;
  end;


implementation

{ TFormatDialog }


procedure TFormatDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

  if fImage is TJPEGImage then
  begin
    with fImage as TJPEGImage do
    begin
      Quality := SpinEdit1.Value;
      Progressive := CheckBox1.Checked;
      Grayscale := CheckBox2.Checked;
      Comment := Edit1.Text;
    end;
  end else

    if fImage is TPNGImage then
    begin
      with fImage as TPNGImage do
      begin
        Filter := ComboBox1.ItemIndex;
      end;
    end else

      if fImage is TBMPImage then
      begin
        with fImage as TBMPImage do
        begin
          UseRLE := CheckBox3.Checked;
        end;
      end else
      {$ifndef OpbCompat}
      if fImage is TTargaGraphic then
      begin
        with fImage as TTargaGraphic do
        begin
          UseRLE := CheckBox3.Checked;
        end;
      end else
      {$endif}

        if fImage is TGIFImage then
        begin
          with fImage as TGIFImage do
          begin
            Interlaced := CheckBox4.Checked;
          end;
        end;

end;

procedure TFormatDialog.FormShow(Sender: TObject);
begin
  if fImage is TJPEGImage then
  begin
    TabJPEG.TabVisible:=true;
    TabBMP.TabVisible := False;
    TabGIF.TabVisible := False;
    TabPNG.TabVisible := False;
    PageControl1.ActivePage:=TabJPEG;
  end else
    if fImage is TPNGImage then
    begin
      TabPNG.TabVisible:=true;
      TabJPEG.TabVisible := False;
      TabGIF.TabVisible := False;
      TabBMP.TabVisible := False;
      PageControl1.ActivePage:=TabPNG;
    end else
      if (fImage is TBMPImage) {$ifndef OpbCompat}or (fImage is TTargaGraphic){$endif} then
      begin
        TabBMP.TabVisible:=true;
        TabJPEG.TabVisible := False;
        TabGIF.TabVisible := False;
        TabPNG.TabVisible := False;
        PageControl1.ActivePage:=TabBMP;
      end else
        if fImage is TGIFImage then
        begin
          TabGIF.TabVisible:=true;
          TabJPEG.TabVisible := False;
          TabPNG.TabVisible := False;
          TabBMP.TabVisible := False;
          PageControl1.ActivePage:=TabGIF;
        end;
end;

initialization
{$I formatdialog.lrs}

end.
