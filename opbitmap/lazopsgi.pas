unit LazOpSGI;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, GraphicEx, Graphics, ClipBrd;

type

  TSharedSGIImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderSGI }

  TFPReaderSGI = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TOPSGIImage }

  TOPSGIImage = class(TFPImageBitmap)
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

{ TOPSGIImage }


class function TOPSGIImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderSGI;
end;

class function TOPSGIImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := nil;
end;

class function TOPSGIImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedSGIImage;
end;


class function TOPSGIImage.GetFileExtensions: string;
begin
  Result:='sgi;rgb';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.sgi');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('sgi', mime.UserLangName, TOPSGIImage);
   TPicture.RegisterFileFormat('rgb', mime.UserLangName, TOPSGIImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPSGIImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPSGIImage);
end;

{ TFPReaderSGI }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderSGI.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TSGIGraphic;
  x, y: integer;
begin
  op := TSGIGraphic.Create;
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



function TFPReaderSGI.InternalCheck(Stream: TStream): boolean;
begin
  Result:=true;
end;

constructor TFPReaderSGI.Create;
begin
  inherited Create;
end;

destructor TFPReaderSGI.Destroy;
begin
  inherited Destroy;
end;


initialization
  Register;

finalization
  UnRegister;

end.
