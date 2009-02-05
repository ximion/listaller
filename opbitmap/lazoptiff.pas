unit LazOpTIFF;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, Graphics, ClipBrd;

type

  TSharedTIFFImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderTIFF }

  TFPReaderTIFF = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TFPWriterTIFF }

  TFPWriterTIFF = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  end;

  TOPTIFFImage = class(TFPImageBitmap)
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

{ TOPTIFFImage }


class function TOPTIFFImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderTIFF;
end;

class function TOPTIFFImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterTIFF;
end;

class function TOPTIFFImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedTIFFImage;
end;


class function TOPTIFFImage.GetFileExtensions: string;
begin
  Result:='tif;tiff';
end;

procedure Register;
var mime:TMimeObject;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.tif');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('tif', mime.UserLangName, TOPTIFFImage);
   TPicture.RegisterFileFormat('tiff', mime.UserLangName, TOPTIFFImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPTIFFImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPTIFFImage);
end;

{ TFPReaderTIFF }


function Pixel24ToFPColor(const a: Pixel24): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := fpImage.alphaOpaque;
end;


procedure TFPReaderTIFF.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TTIFFImage;
  x, y: integer;
begin
  op := TTIFFImage.Create;
  op.LoadFromStream(Stream);
  Img.SetSize(op.Width, op.Height);
  op.PixelFormat := opbitmap.pf24bit;
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      Img.Colors[x, y] := Pixel24ToFPColor(TBitmapData24(Op.Data).NativePixels[x, y]);
    end;
  op.free;
end;



function TFPReaderTIFF.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderTIFF.Create;
begin
  inherited Create;
end;

destructor TFPReaderTIFF.Destroy;
begin
  inherited Destroy;
end;

{ TFPWriterTIFF }

procedure TFPWriterTIFF.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var op: TTIFFImage;
  x, y: integer;
begin
  op := TTIFFImage.Create;
  op.PixelFormat:=opbitmap.pf24bit;
  op.Width := Img.Width;
  op.Height := Img.Height;
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      op.Pixels[x, y] := FPColorToTColor(Img.Colors[x, y]);
    end;
  op.SaveToStream(Stream);
  op.free;
end;

initialization
  Register;

finalization
  UnRegister;

end.
