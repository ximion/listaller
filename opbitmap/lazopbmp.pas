unit LazOpBMP;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, Graphics, ClipBrd;

type

  TSharedBMPImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderBMP }

  TFPReaderBMP = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TFPWriterBMP }

  TFPWriterBMP = class(TFPCustomImageWriter)
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  end;

  { TOPBMPImage }

  TOPBMPImage = class(TFPImageBitmap)
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

{ TOPBMPImage }


class function TOPBMPImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderBMP;
end;

class function TOPBMPImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := TFPWriterBMP;
end;

class function TOPBMPImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedBMPImage;
end;


class function TOPBMPImage.GetFileExtensions: string;
begin
  Result:='bmp';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  cls:=GetGraphicClassForFileExtension('.bmp');
  if cls<>nil then TPicture.UnregisterGraphicClass(cls);
  mime:=MimeMagic.GetMimeInfoFromPattern('*.bmp');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('bmp', mime.UserLangName, TOPBMPImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPBMPImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPBMPImage);
end;

{ TFPReaderBMP }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderBMP.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TBMPImage;
  x, y: integer;
begin
  op := TBMPImage.Create;
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



function TFPReaderBMP.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderBMP.Create;
begin
  inherited Create;
end;

destructor TFPReaderBMP.Destroy;
begin
  inherited Destroy;
end;

{ TFPWriterBMP }


function FPColorToPixel32(const FPColor: TFPColor): Pixel32;
begin
 Result.Red:=FPColor.Red shr 8;
 Result.Green:=FPColor.Green shr 8;
 Result.Blue:=FPColor.Blue shr 8;
 Result.Alpha:=FPColor.Alpha shr 8;
end;



procedure TFPWriterBMP.InternalWrite(Stream: TStream; Img: TFPCustomImage);
var op: TBMPImage;
  x, y: integer;
begin
  op := TBMPImage.Create;
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
