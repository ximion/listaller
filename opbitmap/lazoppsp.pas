unit LazOpPSP;

{$MODE objfpc}{$H+}

interface

uses
  SysUtils, Classes, FPImage, IntfGraphics, opbitmap, opbitmapformats, XPMime, GraphicEx, Graphics, ClipBrd;

type

  TSharedPSPImage = class(TSharedCustomBitmap)
  end;

  { TFPReaderPSP }

  TFPReaderPSP = class(TFPCustomImageReader)
  private

  protected
    procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    function InternalCheck(Stream: TStream): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TOPPSPImage }

  TOPPSPImage = class(TFPImageBitmap)
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

{ TOPPSPImage }



class function TOPPSPImage.GetReaderClass: TFPCustomImageReaderClass;
begin
  Result := TFPReaderPSP;
end;

class function TOPPSPImage.GetWriterClass: TFPCustomImageWriterClass;
begin
  Result := nil;
end;

class function TOPPSPImage.GetSharedImageClass: TSharedRasterImageClass;
begin
  Result:=TSharedPSPImage;
end;

class function TOPPSPImage.GetFileExtensions: string;
begin
  Result:='psp';
end;

procedure Register;
var mime:TMimeObject;
var cls:TGraphicClass;
begin
  mime:=MimeMagic.GetMimeInfoFromPattern('*.psp');
  if Assigned(mime) then
  begin
   TPicture.RegisterFileFormat('psp', mime.UserLangName, TOPPSPImage);
   TPicture.RegisterClipboardFormat(RegisterClipboardFormat(mime.MimeType), TOPPSPImage);
  end;
end;

procedure UnRegister;
begin
  TPicture.UnregisterGraphicClass(TOPPSPImage);
end;

{ TFPReaderPSP }


function Pixel32ToFPColor(const a: Pixel32): TFPColor;
begin
  Result.Red := a.Red + (a.Red shl 8);
  Result.Green := a.Green + (a.Green shl 8);
  Result.Blue := a.Blue + (a.Blue shl 8);
  Result.Alpha := a.Alpha + (a.Alpha shl 8);
end;


procedure TFPReaderPSP.InternalRead(Stream: TStream; Img: TFPCustomImage);
var op: TPSPGraphic;
  x, y: integer;
begin
  op := TPSPGraphic.Create;
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



function TFPReaderPSP.InternalCheck(Stream: TStream): boolean;
begin
  Result := True;
end;

constructor TFPReaderPSP.Create;
begin
  inherited Create;
end;

destructor TFPReaderPSP.Destroy;
begin
  inherited Destroy;
end;


initialization
  Register;

finalization
  UnRegister;

end.
