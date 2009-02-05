unit LazOpCUT;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, GraphicEx, Graphics, ClipBrd;

type

  TSharedCUTImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderCUT }

  TFPReaderCUT = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TOPCUTImage }

  TOPCUTImage = class(TFPImageBitmap)
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

{ TOPCUTImage }


class function TOPCUTImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderCUT;
end;

class function TOPCUTImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := nil;
end;

class function TOPCUTImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedCUTImage;
end;


class function TOPCUTImage.GetFileExtensions: string;
begin
  Result:='cut';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.cut');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('cut', mime.UserLangName, TOPCUTImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPCUTImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPCUTImage);
end;

{ TFPReaderCUT }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderCUT.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TCUTGraphic;
  x, y: integer;
begin
  op := TCUTGraphic.Create;
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



function TFPReaderCUT.InternalCheck(Stream: TStream): boolean;
begin
  Result:=true;
end;

constructor TFPReaderCUT.Create;
begin
  inherited Create;
end;

destructor TFPReaderCUT.Destroy;
begin
  inherited Destroy;
end;


initialization
  Register;

finalization
  UnRegister;

end.
