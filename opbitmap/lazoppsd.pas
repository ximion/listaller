unit LazOpPSD;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, GraphicEx, Graphics, ClipBrd;

type

  TSharedPSDImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderPSD }

  TFPReaderPSD = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TOPPSDImage }

  TOPPSDImage = class(TFPImageBitmap)
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

{ TOPPSDImage }




class function TOPPSDImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderPSD;
end;

class function TOPPSDImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := nil;
end;

class function TOPPSDImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedPSDImage;
end;

class function TOPPSDImage.GetFileExtensions: string;
begin
  Result:='psd';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.psd');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('psd', mime.UserLangName, TOPPSDImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPPSDImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPPSDImage);
end;

{ TFPReaderPSD }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderPSD.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TPSDGraphic;
  x, y: integer;
begin
  op := TPSDGraphic.Create;
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



function TFPReaderPSD.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderPSD.Create;
begin
  inherited Create;
end;

destructor TFPReaderPSD.Destroy;
begin
  inherited Destroy;
end;


initialization
  Register;

finalization
  UnRegister;

end.
