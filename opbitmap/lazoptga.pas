unit LazOpTGA;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, GraphicEx, XPMime, Graphics, ClipBrd;

type

  TSharedTGAImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderTGA }

  TFPReaderTGA = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TFPWriterTGA }

  TFPWriterTGA = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  end;

  { TOPTGAImage }

  TOPTGAImage = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: string; override;
  end;


procedure Register;
procedure UnRegister;

implementation

{ TOPTGAImage }



class function TOPTGAImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderTGA;
end;

class function TOPTGAImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterTGA;
end;

class function TOPTGAImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedTGAImage;
end;


class function TOPTGAImage.GetFileExtensions: string;
begin
  Result:='tga';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.tga');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('tga', mime.UserLangName, TOPTGAImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPTGAImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPTGAImage);
end;

{ TFPReaderTGA }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderTGA.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TTargaGraphic;
  x, y: integer;
begin
  op := TTargaGraphic.Create;
  op.LoadFromStream(Stream);
  Img.SetSize(op.Width, op.Height);
  op.PixelFormat := opbitmap.pf32bit;
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      Img.Colors[x, y] := Pixel32ToFPColor(TBitmapData32(Op.Data).NativePixels[x, y]);
    end;
  op.free;
end;



function TFPReaderTGA.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderTGA.Create;
begin
  inherited Create;
end;

destructor TFPReaderTGA.Destroy;
begin
  inherited Destroy;
end;

{ TFPWriterTGA }


function FPColorToPixel32(const FPColor: TFPColor): Pixel32;
begin
 Result.Red:=FPColor.Red shr 8;
 Result.Green:=FPColor.Green shr 8;
 Result.Blue:=FPColor.Blue shr 8;
 Result.Alpha:=FPColor.Alpha shr 8;
end;



procedure TFPWriterTGA.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var op: TTargaGraphic;
  x, y: integer;
begin
  op := TTargaGraphic.Create;
  op.PixelFormat:=opbitmap.pf32bit;
  op.Width := Img.Width;
  op.Height := Img.Height;
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
     TBitmapData32(Op.Data).NativePixels[x, y]:=FPColorToPixel32(Img.Colors[x, y]);
    end;
  op.SaveToStream(Stream);
  op.free;
end;

initialization
  Register;

finalization
  UnRegister;

end.
