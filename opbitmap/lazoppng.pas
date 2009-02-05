unit LazOpPNG;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, Graphics, ClipBrd;

type

  TSharedPNGImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderPNG }

  TFPReaderPNG = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TFPWriterPNG }

  TFPWriterPNG = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  end;

  TOPPNGImage = class(TFPImageBitmap)
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

{ TOPPNGImage }


class function TOPPNGImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderPNG;
end;

class function TOPPNGImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterPNG;
end;


class function TOPPNGImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedPNGImage;
end;



class function TOPPNGImage.GetFileExtensions: string;
begin
  Result:='png';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  cls:=GetGraphicClassForFileExtension('.png');
  if cls<>nil then TPicture.UnregisterGraphicClass(cls);
  mime:=MimeMagic.GetMimeInfoFromPattern('*.png');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('png', mime.UserLangName, TOPPNGImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPPNGImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPPNGImage);
end;

{ TFPReaderPNG }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderPNG.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TPNGImage;
  x, y: integer;
begin
  op := TPNGImage.Create;
  op.LoadFromStream(Stream);
  op.PixelFormat := opbitmap.pf32bit;
  if op.Transparent then op.SetAlpha(opbitmap.AlphaTransparent);
  Img.SetSize(op.Width, op.Height);
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      Img.Colors[x, y] := Pixel32ToFPColor(TBitmapData32(Op.Data).NativePixels[x, y]);
    end;
  op.free;
end;



function TFPReaderPNG.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderPNG.Create;
begin
  inherited Create;
end;

destructor TFPReaderPNG.Destroy;
begin
  inherited Destroy;
end;

{ TFPWriterPNG }


function FPColorToPixel32(const FPColor: TFPColor): Pixel32;
begin
 Result.Red:=FPColor.Red shr 8;
 Result.Green:=FPColor.Green shr 8;
 Result.Blue:=FPColor.Blue shr 8;
 Result.Alpha:=FPColor.Alpha shr 8;
end;



procedure TFPWriterPNG.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var op: TPNGImage;
  x, y: integer;
begin
  op := TPNGImage.Create;
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
