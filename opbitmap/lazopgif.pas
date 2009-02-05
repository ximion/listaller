unit LazOpGif;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, Graphics, ClipBrd;

type

  TSharedGIFImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderGIF }

  TFPReaderGIF = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TFPWriterGIF }

  TFPWriterGIF = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  end;

  TOPGIFImage = class(TFPImageBitmap)
  protected
    class function GetReaderClass: TFPCustomImageReaderClass; override;
    class function GetWriterClass: TFPCustomImageWriterClass; override;
    class function GetSharedImageClass: TSharedRasterImageClass; override;
  public
    class function GetFileExtensions: string; override;
 //
  end;

{
const
  DefaultGIFMimeType = 'image/gif';    }

procedure Register;
procedure UnRegister;

implementation

{ TOPGIFImage }


class function TOPGIFImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderGIF;
end;

class function TOPGIFImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterGIF;
end;


class function TOPGIFImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result := TSharedGIFImage;
end;


class function TOPGIFImage.GetFileExtensions: string;
begin
  Result := 'gif';
end;

procedure Register;
var mime:TMimeObject;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.gif');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('gif', mime.UserLangName, TOPGIFImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPGIFImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPGIFImage);
end;

{ TFPReaderGIF }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderGIF.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TGIFImage;
  x, y: integer;
begin
  Stream.Position:=0;
  op := TGIFImage.Create;
  op.LoadFromStream(Stream);
  Img.SetSize(op.Width, op.Height);
  op.PixelFormat := opbitmap.pf32bit;
  if Op.Transparent then op.SetAlpha(opbitmap.AlphaTransparent);
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      Img.Colors[x, y] := Pixel32ToFPColor(TBitmapData32(Op.Data).NativePixels[x, y]);
    end;
  op.free;
end;


function TFPReaderGIF.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderGIF.Create;
begin
  inherited Create;
end;

destructor TFPReaderGIF.Destroy;
begin
  inherited Destroy;
end;

{ TFPWriterGIF }

procedure TFPWriterGIF.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var op: TGIFImage;
  x, y: integer;
  haveTransparency: Boolean;
begin
  haveTransparency := false;
  op := TGIFImage.Create;
  op.Width := Img.Width;
  op.Height := Img.Height;
  for y := 0 to op.Height - 1 do
    for x := 0 to op.Width - 1 do
    begin
      op.Pixels[x, y] := FPColorToTColor(Img.Colors[x, y]);
      if not haveTransparency then
        if Img.Colors[x, y].alpha = AlphaTransparent then
        begin
          Op.TransparentColor := FPColorToTColor(Img.Colors[x, y]);
          haveTransparency := true;
        end;
    end;
  op.SaveToStream(Stream);
  op.free;
end;

initialization
  Register;

finalization
  UnRegister;

end.
